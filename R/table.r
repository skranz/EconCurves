example.table = function() {
  ref = c("1A","1B","2","3_4A","4B")
  content = paste0("<p> Cell ",seq_along(ref),"</p>")
  html = html.table(content, ref=ref)

  app = eventsApp()
  app$ui  = fluidPage(
    HTML('<style>
table, th, td {
    border: 1px solid black;
}
</style>'),
    HTML(html))
  runEventsApp(app,launch.browser=rstudio::viewer)
  
}


html.table = function(content, ref=NULL, rc=NULL, nrow=NULL,ncol=NULL) {
  #content = 1:10; nrow=NULL; ncol=2
  restore.point("html.table")
  content = sapply(content, as.character)
  n = length(content)
  
  if (!is.null(ref) & is.null(rc)) {
    rc = ref.to.rowcol(ref)  
  }

  if (is.null(rc)) {  
    if (is.null(nrow) & is.null(ncol)) {
      nrow=n
      ncol=1
    } else if (is.null(nrow)) {
      nrow = ceiling(n/ncol)
    } else if (is.null(ncol)) {
      ncol = ceiling(n/nrow)
    }
    
    txt = sapply(1:nrow, function(r) {
      inds = seq.int(1+(r-1)*ncol,min(r*ncol,n))
      rtxt = paste0("  <td>",content[inds],"</td>",collapse="\n")
      rtxt = paste0("<tr>\n",rtxt,"\n<tr>")
      rtxt
    })
  } else {
    start.rows = unique(rc$start.row)
    
    txt = sapply(start.rows, function(sr) {
      inds = rc$start.row == sr
      irc = rc[inds,,drop=FALSE]
      rtxt = paste0('  <td colspan="',irc$colspan,'" rowspan="',irc$rowspan,'">',
        content[inds],"</td>",collapse="\n")
      rtxt = paste0("<tr>\n",rtxt,"\n</tr>")
      rtxt
    })
  }
  txt = paste0('<table width="100%">',paste0(txt,collapse="\n"),"</table>")
  txt
}


ref.to.rowcol = function(ref) {
  if (is.null(ref)) return(NULL)
  
  col.start = str.locate.first(ref,"[A-Z]", fixed=FALSE)[,1]

  col.start[is.na(col.start)] = nchar(ref[is.na(col.start)])+1
  row.str = substring(ref,1,col.start-1)  
  col.str = substring(ref,col.start)
  
  start.row = as.integer(str.left.of(row.str,"_"))
  end.row = as.integer(str.right.of(row.str,"_"))
  

  start.col = match(str.left.of(col.str,"_"),LETTERS)
  end.col = match(str.right.of(col.str,"_"),LETTERS)
  
  num.rows = max(c(start.row,end.row), na.rm=TRUE)
  num.cols = max(c(start.col,end.col), na.rm=TRUE)
  
  start.row[is.na(start.row)] = 1
  end.row[is.na(end.row)] = num.rows
  
  start.col[is.na(start.col)] = 1
  end.col[is.na(end.col)] = num.cols
  
  rc = data.frame(start.row=start.row, end.row=end.row, start.col=start.col, end.col=end.col)
  rc$colspan = rc$end.col - rc$start.col + 1
  rc$rowspan = rc$end.row - rc$start.row + 1    

  rc
  
}
