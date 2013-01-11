knit_for <- function(n_times, layout) {
  	
	is.latex_inline <- function(item) class(item) == "latex_inline"
	is.loop_chunk <- function(item) class(item) == "loop_chunk" 

	# Error check. 
	# if (any(!sapply(layout, function(l) is.character(l) | is.function(l) | is.latex_inline)))
 # 		stop("Layout must contain only characters or functions.")


  	latex_inline <- function(item, i) {
  
		execute_f <- function(f, i) as.character(f(i))


		name <- item$name

		if (is.list(name)) {
		name_f <- sapply(name, is.function)
		name[name_f] <- lapply(name[name_f], execute_f, i)  
		name_out <- do.call(paste0, name)
		} else if (is.function(name)) {
			name_out <- execute_f(name, i)
		} else {
			stop("Error.") 
		}

		return (paste0("\\", item$type, "{", name_out, "}"))
	  
	}

  	# Create a chunk and shove a function inside.
	chunk_function <- function(chunk, i) {

		# Function arguments. Force there assignment for knitr.
	  	f_arguments <- paste0(names(formals(chunk$f))[1], " <- ", i)    

	  	# Body of the function.
    	f_body <- paste(as.character(body(chunk$f))[-1], collapse="\n")

    	# Combine arguments and body.
	  	f_all <- paste(f_arguments, f_body, sep="\n", collapse="\n")

	  	# Chunk heading with options.
	  	chunk_head <- paste0("<<", chunk$options, ">>=")

	  	# Shove function inside a chunk.
	  	chunk_out <- paste(chunk_head, f_all, "@", sep="\n", collapse="\n")

	  	return(chunk_out)

	}
	
	# Determine which layout elements are functions.
  	layout_functions <- sapply(layout, is.loop_chunk)
	  
  	layout_inline <- sapply(layout, is.latex_inline)

  	# Will eventually contain entire output.
	output <- list()

	for (i in 1:n_times) {
     	  	
     	# Copy the layout so it can be edited.
    	layout_i <- layout
  	
  		# Transform chunks into format suitable for output.
		layout_i[layout_functions] <- sapply(layout_i[layout_functions], chunk_function, i)

		# Transform latex parts into output format.
		layout_i[layout_inline] <- sapply(layout_i[layout_inline], latex_inline, i)

		# Append to overall output.
	 	output[[i]] <- do.call(paste, list(layout_i, collapse="\n"))
	  
	}

	return(output)

}

knit_preview <- function(x, to_file) {
	file_out <- do.call(paste, list(x, collapse="\n\n"))
	file_connection <- file(to_file)
	writeLines(file_out, file_connection)
	close(file_connection)
}


force_assignment <- function(some_object) {

	if (is.list(some_object)) {
		
		object_names <- names(some_object)
		assignments <- character(length(some_object))
		
		for (i in seq(some_object)) {
  			assignments[i] <- paste(object_names[[i]], "<-", deparse((some_object[[i]])))
		}
		
		return (paste(assignments, collapse = "\n"))


	} else {
		return (paste(deparse(substitute(some_object)), "<-", deparse((some_object))))
	}

}

make_tex <- function(type = "section", name) {
  
	if (!is.list(name) & !is.function(name))
		stop("name must be either a list or function.")
 
	return (structure(list(type = type, name = name), 
		.Names = c("type", "name"), class="latex_inline"))
  
}

make_chunk <- function(f, chunk_options=NULL) {

	if (!is.null(chunk_options)) {

		if (!is.list(chunk_options))
			stop("chunk_options must be a list.")

		if (is.null(names(chunk_options[-1]))) 
			stop(paste0("Chunk options must have names. ", 
				"For example:\nlist(label=\"plot-something\", echo=FALSE, results=\"asis\")"))

		chunk_v <- sapply(chunk_options, function(cv) ifelse(is.character(cv), sprintf("'%s'", cv), cv))
		chunk_v <- unlist(chunk_v, use.names = FALSE)

		options_rearrange <- cbind(names(chunk_options), chunk_v, deparse.level = 0)
		options_string <- paste(apply(options_rearrange, 1, paste, collapse=" = "), collapse=", ")	

	} else {

		options_string <- NULL

	}

	return (structure(list(f = f, options = options_string), 
		class="loop_chunk"))	

}

# tex_r <- function(r_code) {
# 	return (paste0("\\Sexpr{knit(text = ", r_code, ")}"))
# }

# tex_li <- function(r_code) {	
# 	return (paste0("###", r_code, "###"))	
# }

# tex_section <- function(name) {
# 	return (paste0("\\section*{", name, "}"))	
# }




####
#	
# Add chunk options as separate list, or probably better, like this:
#
# layout <- list(
# 		list(content = "\\section{Whatever}"},
# 		list(content = fx, options = list(echo = TRUE, results = "asis")),
# 		list(content = "Final bit of LaTeX.")
# 	)
#
# Make it flexible so you can do this:
#	
# layout <- list(
# 		"\\section{Whatever}"
# 		list(content = fx, options = list(echo = TRUE, results = "asis")),
# 		"Final bit of LaTeX."
# 	)
#
# -----------------------------------------------------------------------------------
#
# Support non-numeric loop_through. What if someone wants to do some thing like:
#
# 	for (i in c('a', 'b', 'c'))
#
#
# Probably should have it like this:
#
#	for (i in 1:length(loop_through))
#
# Then evaluate loop_though[i] or loop_through[[i]].
#
# -----------------------------------------------------------------------------------
#
# Chunk options.
#
#