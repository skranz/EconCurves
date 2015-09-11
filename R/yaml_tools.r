examples.find.yaml.block = function() {
  setwd("D:/libraries/EconCurves/EconCurves")
  init.ec()
  ec = get.ec()
  em = load.model("SimpleLabor3Eq")
  yaml = em$yaml
  block = c("vars","w")
  find.yaml.block(yaml, c("vars","w"))
  
  
}

find.yaml.block = function(yaml, block, tab.size=2, sep.lines=TRUE) {
  restore.point("find.yaml.block")
  
  if (sep.lines) yaml = sep.lines(yaml)
  key = block[1]
  lines = which(str.starts.with(yaml,paste0(key,":")))
  if (length(lines)==0) return(NULL)
  
  line = lines[1]
    
  if (line==length(yaml)) {
    end = line
  } else {
    rem.yaml = yaml[(line+1):length(yaml)]
    end.line = (!str.starts.with(rem.yaml, " ")) &
               (!str.starts.with(rem.yaml,"#"))
    end = which(end.line)
    if (length(end)==0) {
      end = length(yaml)
    } else {
      end = line+end[1]-1
    }
  }
  
  if (length(block)>1) {
    block.yaml = substring(yaml[line:end], tab.size+1)
    res = find.yaml.block(yaml=block.yaml,block=block[-1],tab.size=tab.size,sep.lines=FALSE)
    if (is.null(res)) return(NULL)
    return(list(start=res$start+line-1, end=res$end+line-1, yaml=res$yaml))
  } else {
    return(list(start=line, end=end, yaml = yaml[line:end]))
  }
}

replace.yaml.block = function(yaml, new, block,start=NULL, end=NULL,...) {
  restore.point("replace.yaml.block")

  sep.yaml = sep.lines(yaml)
  if (is.null(start)) {
    res = find.yaml.block(sep.yaml,block=block, sep.lines = FALSE,...)
    if (is.null(res)) {
      warning("block not found")
      return(yaml)
    }
    start = res$start
    end = res$end
  }

  res.yaml = c(
    sep.yaml[int.seq(1,start-1)],
    new,
    sep.yaml[int.seq(end+1,length(sep.yaml))]
  )
  merge.lines(res.yaml)
}


add.after.yaml.block = function(yaml, new, block,start=NULL, end=NULL,...) {
  restore.point("add.after.yaml.block")

  sep.yaml = sep.lines(yaml)
  if (is.null(start)) {
    res = find.yaml.block(sep.yaml,block=block, sep.lines = FALSE,...)
    if (is.null(res)) {
      warning("block not found")
      return(yaml)
    }
    start = res$start
    end = res$end
  }
  res.yaml = c(
    sep.yaml[int.seq(1,end)],
    new,
    sep.yaml[int.seq(end+1,length(sep.yaml))]
  )
  merge.lines(res.yaml)
}