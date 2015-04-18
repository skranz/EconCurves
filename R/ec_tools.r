deparse1 = function (call, collapse = "") 
{
    paste0(deparse(call, width = 500), collapse = collapse)
}