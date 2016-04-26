
parse.hashdot.yaml = function(txt, hashdot = "#. ",...) {
  restore.point("parse.hashdot.yaml")
  if (length(txt)==1) txt = sep.lines(txt)
  df = split.text.in.startline.blocks(txt, start.with = "#. ")
  li = list()
  start.txt = df$inner[1]
  if (nchar(start.txt)>0) {
    li = read.yaml(text = start.txt)
  }
  if (NROW(df)>1) {
    df = df[-1,]
    names = str.right.of(df$head, hashdot) %>% str.left.of(" ")
    li[names] = as.list(df$inner)
  }
  li
} 

split.text.in.startline.blocks = function(txt, start.with = NULL, block.lines = NULL, add.start=TRUE, merge.lines=TRUE) {
  restore.point("split.text.in.blocks")
  
  if (length(txt)==0) return(NULL)
  if (is.null(block.lines)) {
    block.lines =  which(str.starts.with(txt,hashdot))
  }
  if (length(block.lines)==0) {
    if (!add.start) return(NULL)
    
    if (merge.lines) {
      inner = paste0(txt, collapse="\n")
    } else {
      inner = txt    
    }
    return(data_frame(start=1, end = length(txt), head="", inner=inner))
  }
  
  start = block.lines
  end = c(block.lines-1, length(txt))[-1]
  if (!merge.lines) {
    inner = lapply(seq_along(start), function(i) {
      txt[(start[i]+1):end[i]]
    })
  } else {
    inner = sapply(seq_along(start), function(i) {
      paste0(txt[(start[i]+1):end[i]], collapse="\n")
    })
  }
  head = txt[start]
  if (add.start) {
    if (start[1]==1) {
      start = c(0,start)
      end = c(0,end)
      head = c("START",head)
      inner = c("",inner)
    } else {
      end = c(start[1]-1,end)
      start = c(0,start)
      head = c("START",head)
      if (merge.lines) {
        inner = c(merge.lines(txt[ 1:end[1] ]),inner)       
      } else {
        inner = c(list(txt[ 1:end[1] ]),inner)       
      }
    }
  }
  data_frame(start=start,end=end, head=head, inner=inner)
}
