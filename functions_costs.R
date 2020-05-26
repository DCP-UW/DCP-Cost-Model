#######################
## CALCULATING COSTS ##
#######################
# Functions------------
tradable_conversion <- function(unit_cost, tradable_ratio, currency, global_inflation, exchange){
  if(currency == "USD") {
    unit_cost*tradable_ratio*global_inflation} else {
      ((unit_cost*tradable_ratio)/exchange)*global_inflation
    }
}

nontradable_conversion <- function(unit_cost,tradable_ratio, currency, cpi_adjust, exchange,country, exchange_end, gni, gni_selected_country){
  if(currency == "USD" & (country == "LIC" | country == "LMIC")) {
    unit_cost*(1-tradable_ratio)*cpi_adjust
  } else if (currency == "USD" & (country != "LIC" & country != "LMIC")){
    ((unit_cost*exchange)*(1-tradable_ratio))*cpi_adjust/exchange_end*(gni_selected_country/gni)} else {
      ((unit_cost*(1-tradable_ratio)*cpi_adjust)/exchange_end)*(gni_selected_country/gni)
    }
}


datatable2 <- function(x, vars = NULL, opts=NULL, ...) {
  
  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(map_chr(x[, pos], typeof) == "list"))
    stop("list columns are not supported in datatable2()")
  
  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(' ' = '&oplus;', x)
  
   options
  opts <- c(
   opts, 
   list(
     columnDefs = list(
       list(visible = FALSE, targets = c(0, pos)),
       list(orderable = FALSE, className = 'details-control', targets = 1),
       list(className = 'dt-left', targets = 1:3),
       list(className = 'dt-right', targets = 4:ncol(x))
      )
    )
  )
  
  datatable(
    x, 
    ...,
    escape = -2,
    options = opts ,
    callback = JS(.callback2(x = x, pos = c(0, pos))),
    editable = list(target='cell', disable=list(columns=c(1:13)))
  )
}


.callback2 <- function(x, pos = NULL) {
  
  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"
  
  part2 <- .child_row_table2(x, pos = pos)
  
  part3 <- 
    "
  table.on('click', 'td.details-control', function() {
  var td = $(this), row = table.row(td.closest('tr'));
  if (row.child.isShown()) {
  row.child.hide();
  td.html('&oplus;');
  } else {
  row.child(format(row.data())).show();
  td.html('&ominus;');
  }
  });"
    
  paste(part1, part2, part3)
} 

.child_row_table2 <- function(x, pos = NULL) {
  
  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
  text = '<div><table >' + 
  "
  
  for (i in seq_along(pos)) {
    text <- paste(text, glue::glue(
      "'<tr>' +
      '<td>' + '{names_x[pos[i]]}' + '</td>' +
      '<td>' + d[{pos[i]}] + '</td>' +
      '</tr>' + " ))
  }
  
  paste0(text,
         "'</table></div>'
         return text;};"
  )
  }