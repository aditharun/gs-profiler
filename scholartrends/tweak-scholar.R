getresp <- function (url, attempts_left = 5) 
{
    stopifnot(attempts_left > 0)
    resp <- httr::GET(url)
    if (httr::status_code(resp) == 200) {
        resp
    }
    else if (httr::status_code(resp) == 404) {
        warning("Page 404. Please check whether the provided URL is correct.")
        return(NULL)
    }
    else if (httr::status_code(resp) == 429) {
        warning("Response code 429. Google is rate limiting you for making too many requests too quickly.")
        return(NULL)
    }
    else if (attempts_left == 1) {
        warning("Cannot connect to Google Scholar. Is the ID you provided correct?")
        return(NULL)
    }
    else {
        Sys.sleep(1)
        get_scholar_resp(url, attempts_left - 1)
    }
}


environment(getresp) <- asNamespace('scholar')
assignInNamespace("get_scholar_resp", getresp, ns = "scholar")