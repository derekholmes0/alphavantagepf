#' @import promises
#' @import ellmer
#' @importFrom mcptools mcp_tools
create_chat_instance <- function() {
  message_if_red(TRUE,"Instantiating Claude Chat, with AV tools")
  # collect system prompts
  sys_prompt_sys <- ellmer::interpolate_file(
    system.file("prompts", "prompt.md", package = "alphavantagepf"),
    max_returned_rows=200
  )
  sys_prompt_user_fn <- paste0( the_av$defaultcachedir, "/avpf_sysprompt_user.md" )
  sys_prompt_user <- ifelse( file.exists( sys_prompt_user_fn ), ellmer::interpolate_file(sys_prompt_user_fn), "")
  # Create chat
  av_chat <- ellmer::chat_anthropic(
    system_prompt = paste(sys_prompt_sys, sys_prompt_user),
    model = the_av$llm_model,
    echo="all")
  # Add tools
  av_tools <- mcp_tools(paste0(the_av$defaultcachedir,"/config.json"))
  av_tools <- lapply(av_tools, av_fix_tool_result)

  #av_chat$set_tools(c(av_chat$get_tools(),av_tools))
  av_chat$set_tools(av_tools)
  cAssign("av_tools")
  cAssign("av_chat;sys_prompt_user;sys_prompt_sys")
  message_if_red(TRUE,"Chat (",av_chat$get_model(),") initiated")
  return(av_chat)
}

# Never could  have done this without Claude
av_fix_tool_result <- function(td) {
  new_fn <- function(...) {
    result <- td(...)  # call the original tool
    if (inherits(result, "ellmer::ContentToolResult") &&
        is.list(result@value) &&
        !is.null(result@value$result)) {
      result@value <- result@value$result
    }
    result
  }
  attributes(new_fn) <- attributes(td)  # restore class, name, description, arguments, etc.
  new_fn
}

get_last_user_question <- function(chat) {
  last_turns <- chat$get_turns() %||% { return("--") }
  user_turns <- Filter(\(t) t@role == "user", last_turns)
  if(length(user_turns)<=0) { return("")}
  ellmer::contents_text(user_turns[[length(user_turns)]])
}


# ====================================== TOOOLS =======================================
# ====================================== TOOOLS =======================================
# ====================================== TOOOLS =======================================


#' @import ellmer
#' @importFrom mcptools mcp_tools
avt_get_pxd <- function(symbol, field) {
  if( nrow(the_av$pxinv[symbol==symbol,])<=0 ) {
    return( list(status="Ticker Not Found", symbol=symbol, field=field) )
  }
  if( !(field %in% names(the_av$pxd)) ) {
    return( list(status="Field Not Found", symbol=symbol, field=field) )
  }
  thisdt = the_av$pxd[symbol==symbol,.SD,.SDcols=c("timestamp",field)]
  return( list(status="ok", symbol=symbol, field=field, value=thisdt, as_of=format(max(thisdt$ts))) )
}

#avtool_get_pxd <- ellmer::tool( avt_get_pxd, name="avt_get_pxd", description)
