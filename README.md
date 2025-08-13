# stefan

A quick, dirty, and unsafe package to run R code on a remote server via SSH.
Tries to mimic local workspaces in remote sessions including parent environments,
package namespaces, standard error/output and tracebacks. Can sometimes be useful
if you have expensive code that you don't want to run on your machine but
cluster parallelization doesn't work.

> [!IMPORTANT]  
> The package does not perform any code sanitation. This means that harmful
> code can be run on a remote server. Also, the package automatically grabs
> your R workspace and sends it to a remote server, so take care when working
> with sensitive data!


Install from Github:

```r
pak::pkg_install("jslth/stefan")
```


Make sure you provide your server information. If you can use them to connect
via `connect()`, you're good to go.

```r
options(stefan_server = ...)
options(stefan_user = ...)
```

Use it like this:

```r
remotely(str(list(a = 1, b = 2)))
#> List of 2
#>  $ a: num 1
#>  $ b: num 2
```

You can also attach packages remotely. Note, however, that unavailable packages
must be installed first, either manually using `rmt_install()` or by allowing
auto-installing using `remotely(..., allow_install = TRUE)`.

```r
remotely(file_ext("test.csv"), pkgs = "tools")
#> [1] "csv"
```

Caller environments (and their non-package parents) are automatically sent with the request.

```r
x <- 1:10
remotely(mean(x))
[1] 5.5
```

Errors are automatically signaled locally with their tracebacks intact (only works with rlang errors right now).

```r
remotely({
  error_fun <- function() rlang::abort("This is an error!")
  error_fun()
})
#> Error in `error_fun()`:
#> ! [remote] This is an error!
#> Run `rlang::last_trace()` to see where the error occurred.

rlang::last_trace(drop = FALSE)
#> <error/rlang_error>
#> Error in `error_fun()`:
#> ! [remote] This is an error!
#> ---
#> Backtrace:
#>      ▆
#>   1. ├─base::local(...)
#>   2. │ └─base::eval.parent(substitute(eval(quote(expr), envir)))
#>   3. │   └─base::eval(expr, p)
#>   4. │     └─base::eval(expr, p)
#>   5. └─base::eval(...)
#>   6.   └─base::eval(...)
#>   7.     ├─base::withVisible(...)
#>   8.     ├─base::local(...)
#>   9.     │ └─base::eval.parent(substitute(eval(quote(expr), envir)))
#>  10.     │   └─base::eval(expr, p)
#>  11.     │     └─base::eval(expr, p)
#>  12.     └─base::eval(...)
#>  13.       └─base::eval(...)
#>  14.         └─error_fun()
#>  15.           └─rlang::abort("This is an error!")
```

Standard error and standard output are caught.

```r
remotely({
  message("this is a message")
  warning("this is a warning")
  print(head(mtcars))
})
#> this is a message
#> Warning in eval(quote({ : this is a warning
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
```

Some things that don't and will likely never work:

- Graphics devices
- External pointers
- Open connections (e.g., `file()` or databases)
- Local file system paths
- Interactive input (e.g., `readline()`)
- Package version control (e.g., saying that you're working with version 1.1 rather than the latest one)
- Startup scripts (.Rprofile, .Renviron)
- Environment inheritance (environment objects are dumped into one main environment)
- Probably a lot more
