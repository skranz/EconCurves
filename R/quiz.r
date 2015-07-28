examples.quiz = function() {
    yaml = '
parts:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = parse.quiz.yaml(yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)
  
  runEventsApp(app, launch.browser=rstudio::viewer)
  
  
}

parse.quiz.yaml = function(yaml,quiz.id =paste0("quiz_",sample.int(10e10,1))) {
  restore.point("parse.quiz.yaml")
  library(YamlObjects)
  qu = read.yaml(text=yaml)
  init.quiz(qu, quiz.id)
}

init.quiz = function(qu, quiz.id=paste0("quiz_",sample.int(10e10,1))) {
  if (is.null(qu[["id"]])) {
    qu$id = quiz.id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }
  
  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu))
  
  qu    
  
}

init.quiz.part = function(part, part.ind=1, qu=NULL) {
  restore.point("init.quiz.part")
  
  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)
    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=0
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  } 
  
  expl = part[["expl"]]
  if (!is.null(expl))
    expl = markdownToHTML(text=expl,encoding = "UTF-8", fragment.only=TRUE)
  
  if (is.null(part$success)) {
    part$success = paste0("<p><b>Correct.</b>",expl,"</p>")
  } else {
    part$success =  markdownToHTML(text=part$success,encoding = "UTF-8", fragment.only=TRUE)
  }
  if (is.null(part$failure)) {
    part$failure = paste0("<p><b>Not correct.</b>",expl,"</p>")
  } else {
    part$failure =  markdownToHTML(text=part$failure,encoding = "UTF-8", fragment.only=TRUE)
  }
  
  
  part$id = paste0(qu$id,"__part", part.ind) 
  part$inputId = paste0(part$id,"__answer")
  part$checkBtnId = paste0(part$id,"__checkBtn")
  part$explId = paste0(part$id,"__explainUI")
  part$ui = quiz.part.ui(part)

  
  part
}

quiz.ui = function(qu, in.well.panel=TRUE) {
  
  if (in.well.panel) {
    pli = lapply(qu$parts, function(part) {
      wellPanel(part$ui)
    })
  } else {
    pli = lapply(qu$parts, function(part) {
      part$ui
    })
  }
  pli
}

quiz.part.ui = function(part) {
  head = list(
    HTML(paste0("<hr>",part$question))
  )
  if (part$type=="numeric") {
    answer = numericInput(part$inputId, label = "",value = NULL)  
  } else if (part$type =="text") {
    answer = textInput(part$inputId, label = "",value = "")  
  } else if (part$type=="mc") {
    answer = checkboxGroupInput(part$inputId, "",part$choices)
  } else if (part$type=="sc") {
    answer = radioButtons(part$inputId, "",part$choices, selected=NA)
  }
  
  button = bsButton(part$checkBtnId,label = "check", size="small")
  setUI(part$explId,NULL)
  list(head,answer,button, uiOutput(part$explId))
  
}

add.quiz.handlers = function(qu, check.fun=NULL, set.ui=TRUE){
  restore.point("add.quiz.handlers")
  for (part in qu$parts) {
    buttonHandler(part$checkBtnId,fun = click.check.quiz, part=part, qu=qu, check.fun=check.fun, set.ui=set.ui)
  }
}

click.check.quiz = function(app=getApp(), part, qu,check.fun=NULL, set.ui=TRUE, tol=1e-8, ...) {
  answer = getInputValue(part$inputId)
  restore.point("click.check.quiz")
  
  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<=max(part$roundto,tol))
  } else {
    correct = setequal(answer,part$answer)
  }
  
  if (set.ui) {
    if (correct) {
      cat("Correct!")
      setUI(part$explId,HTML(part$success))
    } else {
      cat("Wrong")
      setUI(part$explId,HTML(part$failure))
    }
  }
  if (!is.null(check.fun)) {
    check.fun(qu=part,answered=TRUE,correct=correct)
  }
}



init.statement = function(sta, qu.ind=1) {
  restore.point("init.statement")
  types = substring(names(sta),1,1)
  yn.inds = which(types=="y" | types=="n")
  yn.ind = sample.int(length(yn.inds),1)

  st = list(qu.ind=qu.ind,yn.ind=yn.ind, yn = sta[[yn.ind]], type=types[[yn.ind]],
    all.yn=sta[yn.inds],types=types,expl=sta$e)
  st
}

add.quiz.ui.id = function(qu, qu.ind = qu$qu.ind) {
  qu$explId = paste0("quiz_explain__", qu.ind)
  qu$checkBtnId = paste0("quiz_checkBtn__", qu.ind)
  qu$inputId = paste0("quiz_input__", qu.ind)
  qu
}

statement.ui = function(st, check.fun=NULL, choices=c("true","false")) {
  restore.point("statement.ui")
  st = add.quiz.ui.id(st)
  
  ch = list("true","false")
  names(ch) = choices
  
  input = radioButtons(st$inputId, label="", choices=ch, selected=FALSE, inline=TRUE)
  ui = list(
    HTML(paste0("<hr>",st$yn)),
    input,
    bsButton(st$checkBtnId,label = "check",size = "extra-small"),
    uiOutput(st$explId)
  )
  buttonHandler(st$checkBtnId, check.statement.handler,st=st,check.fun=check.fun)
  setUI(st$explId,NULL)
  ui
}

check.statement.handler = function(st, check.fun,...) {
  restore.point("check.statement.handler")
  
  value = getInputValue(st$inputId)
  if (!(isTRUE(value=="false") | isTRUE(value=="true") )) {
    check.fun(answered=FALSE, correct=NA,qu=st,...)
    return()
  }
  if ( (value=="true" & st$type=="y") |
       (value=="false" & st$type=="n") ) 
  {
    check.fun(answered=TRUE,correct=TRUE, qu=st,...)
  } else {
    check.fun(answered=TRUE,correct=FALSE, qu=st,...)
  }
}