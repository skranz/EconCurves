
compute.step.shows = function(step, given_show=NULL, all.names=names(pane$objs),data_rows=pane$data_rows, pane=NULL) {
  restore.point("compute.step.preshow.postshow")
  
  if (!is.null(given_show) & !is.list(given_show))
    given_show = compute.show.list(given_show, data_rows=data_rows, all.names=all.names)
  
  preshow = postshow = compute.show.list(hide = step$hide, data_rows=data_rows, all.names=all.names, given_show = given_show)  
  
  
  task.show = c(postshow,step[["find"]], step[["find_shift"]])
  
  postshow = compute.show.list(task.show, data_rows=data_rows, all.names=all.names, given_show = postshow)
  
  postshow = compute.show.list(step$show_success, data_rows=data_rows, all.names=all.names, given_show = postshow)
  
  postshow = compute.show.list(hide = step$hide_success, data_rows=data_rows, all.names=all.names, given_show = postshow)  
  
  nlist(preshow, postshow)
  
}

compute.show.list = function(show=NULL, hide=NULL, data_rows=1, given_show=NULL, all.names=names(pane$objs), pane=NULL) {
  restore.point("compute.show.list")
  
  if (is.null(show) & is.null(hide)) return(given_show)
  
  n = length(data_rows)
  
  role.names = names(data_rows)
  if (is.null(role.names))
    role.names = as.character(data_rows)
  
  
  
  if (identical(show,".none")) {
    show = lapply(data_rows, function(dr) NULL)
    names(show) = role.names
    return(show)
  }
  
  show = show.hide.to.list(show, all.names = all.names, role.names = role.names)
  
  if (!is.null(given_show)) {
    show = lapply(seq_along(show), function(i) {
      unique(c(show[[i]], given_show[[i]]))
    })
  }
  if (!is.null(hide)) {
    hide = show.hide.to.list(hide, all.names = all.names, role.names = role.names)
    show = lapply(seq_along(show), function(i) {
      setdiff(show[[i]], hide[[i]]))
    })
  }
  show
}

show.hide.to.list = function(sh, all.names, role.names="1") {
  restore.point("show.hide.to.list")
  if (is.list(sh)) return(sh)

  if (identical(sh,".all")) {
    li = lapply(role_names, function(dr) all.names)
    names(li) = role.names
    return(li)
  }

  all.sh = sh[sh %in% all.names]
  
  li = lapply(seq_along(role.names), function(i) {
    dr.ind = sh %in% paste0(all.names,"_",role.names[i])
    unique(all.sh,all.names[dr.ind])
  }
  names(li) = role.names
  li
  
}  
  