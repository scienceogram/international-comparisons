################################################################################
###  VARIABLE DEFINITION  ######################################################
################################################################################
default.logfile.name <- 'log.txt'

################################################################################
###  DATA FRAMES  ##############################################################
################################################################################

sample.df <- function(df, size = 1, replace = FALSE, prob = NULL) {
  # Samples rows from a data frame.
  #
  # Args:
  #      df: A data frame.
  #   size, replace, prob: Arguments from the sample function.
  #
  # Returns:
  #   size rows from this data frame (default 1), with or without replacement
  #   and with an optional probability vector.
  df[sample(nrow(df), size, replace, prob), ]
}

withoutCols <- function(df, cols) {
  # Returns a vector to allow certain columns to be excluded from a
  # data frame. The case where the columns being excluded are referred to
  # numerically is trivial, but is included as well for generality.
  #
  # Args:
  #      df: a data frame.
  #    cols: the columns to be excluded, as a vector or scalar of number(s) or
  #          name(s).
  #
  # Returns:
  #      In the text case, a vector of booleans; TRUE for columns to include.
  #      In the numerical case, R understands df[-3,], so just minus the input.
  if(is.character(cols)) { return(!(names(df) %in% cols)) }
  else { return(-cols) }
}

explodeByCol <- function(df, cols, sep=',', regex = NULL,
                         fixed = TRUE) {
  # If your data frame contains multiple values in a single column, this splits
  # multiple values across different rows, using either a separator character or
  # a regular expression.
  #
  # Args:
  #       df: a data frame.
  #     cols: one or more column(s) to explode by.
  #      sep: the separator of the multiple values.
  #    regex: a regular expression which matches the values you're looking for;
  #           overrides sep.
  #    fixed: for strsplit, this variable determines whether the string passed
  #           in sep is fixed (TRUE) or a regular expression (FALSE).
  #
  # Returns:
  #      A data frame with new rows, one for each value in the exploded column.
  
  # data frames fairly often come in with 'character' columns which are factors,
  # and these string-based functions can't handle that, so convert with a
  # warning
  
  # instantiate a list to contain the exploded output
  exploded <- list()
  n.exploded <- list()
  
  for(col in cols) {
    if(is.factor(df[, col])) {
      warning(
        paste0('Column ', col, ' is a factor, and has been coerced ',
               'to a character for exploding.')
      )
      df[, col] <- as.character(df[, col])
    } else if(!is.character(df[, col])) {
      # if it's not character data, it won't work, so pass an error
      stop(
        paste0('Column  ', col, ' passed to explodeByType should be character ',
               'data; it is of class ', class(df[, col]), '.'
        )
      )
    }
    # if regex is NULL, use the separator provided
    if(is.null(regex)) {
      exploded[col] <- list(strsplit(df[, col], sep, fixed = fixed))
      # otherwise, use a regular expression to split the column
    } else {
      exploded[col] <- list(regmatches(df[, col], gregexpr(regex, df[, col])))
    }
    # how many of each row should I create? ie 1,1,2,1,0
    n.exploded[[col]] <- sapply(exploded[[col]], length)
  }
  
  # check the n.exploded values are the same for all columns
  if(!allSame(n.exploded)) {
    stop(
      paste0('The columns provided have inconsistent numbers of elements ',
             'after exploding.'
      )
    )
  }
  # turn the first element of n.exploded into a list of data frame row indices,
  # ie 1,2,3,3,4
  n.exploded.rows <- rep(1:length(n.exploded[[cols[1]]]), n.exploded[[cols[1]]])
  
  # take the data frame and repeat rows the relevant number of times
  df <- df[n.exploded.rows, ]
  # fill its exploded column(s) with the appropriate values
  for(col in cols) {
    df[, col] <- unlist(exploded[[col]])
  }
  df
}

################################################################################
###  CHARACTERS  ###############################################################
################################################################################

removeWhitespace <- function(x) { gsub("\\s","", x) }
# For a character or vector of characters x, removes all spaces and line
# breaks.
#
# Args:
#      x: A character or vector of characters.
#
# Returns:
#      The character with whitespace removed.

pastePlus <- function(..., sep=" ", collapse = NULL, recycleZeroLength = TRUE) {
  # Version of the base R paste function which optionally returns nothing if any
  # of the ...s being concatenated have zero length. (Default behaviour is to
  # recycle them to "".)
  #
  # Args:
  # ..., sep, collapse: as paste in base R
  #   ignoreZeroLength: 
  #
  # Returns:
  #      If any of the passed objects has zero length, NULL; otherwise, the
  #      result of the paste function.
  if(!recycleZeroLength &
     any(lapply(list(...), length) == 0)) {
    return(NULL);
  }
  paste(..., sep = sep, collapse = collapse)
}

paste0Plus <- function(..., collapse = NULL, recycleZeroLength = TRUE) {
  pastePlus(..., sep="", collapse = collapse,
            recycleZeroLength = recycleZeroLength)
}

strPos <- function(..., fixed = TRUE) {
  # Wrapper function which returns the positions of the first occurrence of a
  # pattern in some text. Simplifies regexpr which returns a variety of things
  # other than simply the position. Defaults to fixed rather than regular
  # expression searching.
  #
  # Args:
  #    ...: see grep in base R; usually (pattern, text)
  #  fixed: Logical. If true, pattern is matched as-is.
  #
  # Returns:
  #      The position of the first occurrence of the pattern in the text.
  regexpr(..., fixed = fixed)[1]
}

randomString <- function(l, characters = letters, disallowed = NULL) {
  # Generate a random string, with optional excision of disallowed sequences.
  #
  # Args:
  #      l: The length of the string to generate in number of components
  #         (usually single characters, see below)
  #  characters: Either a string eg 'Argh' which will be split into individual
  #         characters to act as string components, or a vector of components;
  #         no check is made so these can be multi-character
  #  disallowed: A vector of disallowed sequences. Defaults to NULL, which
  #         lets anything through.
  #
  # Returns:
  #      A string of length l picked from the characters provided, without any
  #      disallowed strings.
  
  
  # if passed a single-element vector, it's almost certain they don't want a
  # single string repeated 'randomly' over and over, so split it into characters
  if(length(characters == 1)) {
    characters <- unlist(strsplit(characters, ''))
  }
  # generate a random string by sampling from characters
  random.string <- paste0(
    sample(
      characters,
      l,
      replace = TRUE
    ),
    collapse=''
  )
  # if they've passed a disallowed vector, let's check none of the parts of the
  # string contain it
  if(!is.null(disallowed)) {
    # loop over elements of disallowed
    for(not.allowed in disallowed) {
      # find matches of the forbidden string
      not.allowed.matches <- gregexpr(not.allowed, random.string)
      # if some matches are found...
      while(not.allowed.matches[[1]][1] != -1) {
        # ...loop over them, getting rid of one at a time
        for(i in 1:length(not.allowed.matches[[1]])) {
          substring(
            random.string,
            # start at the match point
            not.allowed.matches[[1]][i],
            # end at match point plus match length
            not.allowed.matches[[1]][i] + attr(not.allowed.matches[[1]], 'match.length')[i]
          ) <-
            # and what better to replace them with than a string generated at
            # random with this very function!
            randomString(
              attr(not.allowed.matches[[1]], 'match.length')[i],
              characters,
              disallowed
            )
        }
        # and then perform the test again to make sure we didn't introduce
        # any unexpected disallowed patterns with the replacements...
        not.allowed.matches <- gregexpr(not.allowed, random.string)
      }
    }
  }
  # return the random string
  random.string
}

randomStrings <- function(n, l, characters = letters, disallowed = NULL) {
  # Generate n random strings; wrapper for the randomString function.
  #
  # Args:
  #      n: Number of random strings to generate
  #    ...: For other arguments, see randomString
  #
  # Returns:
  #      n random strings with the specified properties
  replicate(n, randomString(l, characters, disallowed))
}

################################################################################
###  FACTORS  ##################################################################
################################################################################

concatFactors <- function(...) {
  # Takes some factors and concatenates them. R coerces factors to integers if
  # you don't convert them to character vectors at the intermediate stage, so
  # this saves typing that every time.
  #
  # Args:
  #      ...: Some factors
  #
  # Returns:
  #      A big factor.
  factor(unlist(lapply(list(...), FUN=as.character)))
}

factorChooseFirst <- function(x, first) {
  # Move a chosen level to be the first in a factor.
  #
  # Args:
  #         x: A factor.
  #     first: The level in the factor you want to be first.
  #
  # Returns:
  #      A factor with the first level redefined to be the one specified.
  
  # if the level requested to be first isn't present, this ain't gonna work
  if (!(first %in% levels(x))) {
    stop(paste("Error: the level", first, "doesn't appear in the factor",
               deparse(substitute(x))))
  }
  factor(x, levels = c(first, levels(x)[levels(x) != first]))
}

factorNAfix <- function(x, NAval = 'NA', force = FALSE) {
  # Make NA values in a factor into their own level.
  #
  # Args:
  #         x: A factor.
  #     NAval: The value to replace NAs with. The string 'NA' by default.
  #     force: Whether to force the operation even if there aren't any NAs in
  #            the passed factor.
  #
  # Returns:
  #      A factor with NAs replaced by a specific level.
  
  # if it's forced, or if it's not but there are NAs present...
  if(force | sum(is.na(x)) > 0) {
    levels(x) <- c(levels(x), NAval)
    x[is.na(x)] <- NAval
  }
  x
}

allSame <- function(x) {
  # Work out whether all elements of a list or vector are the same.
  #
  # Args:
  #         x: A list or vector. Works if the list's elements are themselves
  #            lists or vectors.
  #
  # Returns:
  #      TRUE or FALSE, depending.
  length(unique(x)) == 1
}

allSameLength <- function(x) {
  # Work out whether all elements of a list are the same length.
  #
  # Args:
  #         x: A list.
  #
  # Returns:
  #      TRUE or FALSE, depending.
  length(unique(lapply(x, length))) == 1
}

################################################################################
###  LISTS  ####################################################################
################################################################################

list2dataframe <- function(x)  {
  # Simple wrapper to very naively turn a list into a data frame. If your list
  # elements have different numbers of elements, this will go wrong!
  #
  # Args:
  #      x: A list.
  #
  # Returns:
  #      A data frame made from the passed list.
  data.frame(matrix(unlist(x), ncol = length(x[[1]]), byrow = TRUE))
}

################################################################################
###  FILES  ####################################################################
################################################################################

list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      ignore.case=FALSE) {
  # Lists the directories present within a path.
  # Credit: http://stackoverflow.com/questions/4749783
  #
  # Args:
  #      See list.files
  #
  # Returns:
  #      A vector of directories within the path being searched.
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  all[file.info(all)$isdir]
}

writeTablePlus <- function(data, filename, comment='', sep='\t',
                           comment.char='#', col.names=NA,  ...) {
  # A wrapper for the write.table function which adds a comment of your choice
  # at the top of the file.
  #
  # Args:
  #     filename: The name of the file to be written.
  #      comment: The comment to be added at the top of the file.
  #          sep: The separator for the data, tab by default.
  # comment.char: The character denoting comments, # by default.
  #    col.names: write.able argument. The NA default here ensures that the
  #               header row is correctly offset given that the first column is
  #               row names.
  #         ... : Allows arbitrary extra arguments relevant to write.table.
  #
  # Returns:
  #      Nothing!
  f <- file(filename, open="wt") # open a connection to the file
  # if there's a comment, write that first
  if(nchar(comment) > 0) {
    # wrap the comment at 80 lines prefixed the comment character plus space
    comment <- strwrap(comment, width=80, prefix=paste(comment.char,' ',sep=''))
    writeLines(comment, f)
  }
  write.table(data, f, sep=sep, col.names=col.names, ...)
  close(f)
}

readTablePlus <- function(filename, sep='\t', comment.char='#', header=TRUE,
                          ...) {
  # Handy wrapper for the read.table function to make it compatible with the
  # writeTablePlus function with its default options.
  #
  # Args:
  #  filename: The name of the file to read. If it's a vector, the function
  #            read the list of files provided and concatenate them into a
  #            single data frame.
  if(length(filename) > 1) {
    do.call(rbind, lapply(filename, readTablePlus))
  } else {
    read.table(filename, sep=sep, comment.char=comment.char, header=header, ...)
  }
}

justFilename <- function(x) {
  # Returns filenames without extensions.
  #
  # Args:
  #       x: A character or vector of characters containing filenames, with or
  #          without paths.
  #
  # Returns:
  #   The string or vector with everything after and including a final full stop
  #   removed.
  sapply(strsplit(basename(x),"\\."),
         function(x) paste(x[1:(length(x)-1)],
                           collapse=".")
  )
}

fileExt <- function(x) {
  # Returns just extensions from filenames.
  #
  # Args:
  #       x: A character or vector of characters containing filenames, with or
  #          without paths.
  #
  # Returns:
  #   Everything after a final full stop.
  
  # split the strings by full stops, and only take the final element
  extensions <- sapply(strsplit(basename(x),"\\."),
                       function(x) tail(x, 1)
  )
  # where the extension is the same as the input filename, there is no extension
  extensions[extensions == x] <- ''
  
  extensions
}

suffixFilename <- function(x, suffix = '_1') {
  # Returns a filename with a suffix appended before its extension.
  #
  # Args:
  #       x: A character or vector of characters containing filenames.
  #
  # Returns:
  #   filename_suffix.ext
  paste0(justFilename(x), suffix, '.', fileExt(x))
}

################################################################################
###  MATHEMATICS  ##############################################################
################################################################################

floorPlus <- function(x, digits = 0) {
  # the function floor but with a "digits" option to floor not just to the
  # nearest integer
  #
  # Args:
  #         x: vector to floor
  #         digits: Number of decimal places to be used
  #
  # Returns:
  #   integer/vector.
  floor(x*(10^digits)) / 10^digits
}

ceilPlus <- function(x, digits = 0) {
  # the function ceil but with a "digits" option to floor not just to the
  # nearest integer
  #
  # Args:
  #         x: vector to floor
  #         digits: Number of decimal places to be used
  #
  # Returns:
  #   integer/vector.
  ceil(x*(10^digits)) / 10^digits
}

tri <- function(x) {
  # Calculates the xth triangular number.
  #
  # Args:
  #       x: A number.
  #
  # Returns:
  #   The xth triangular number.
  x * (x + 1) / 2
}

trirt <- function(x) {
  # Calculates the triangular root of a number.
  #
  # Args:
  #       x: A number.
  #
  # Returns:
  #   Its triangular root.
  (sqrt(8*x + 1) - 1) / 2
}

# A series of functions which allow arithmetic on quantities with uncertainty.
# Create a quantity by passing values or vectors to unum(x, dx), and then add,
# subtract, multiply or divide with the functions below.
unum <- function(x, dx) { data.frame(x=x, dx=dx) }
# Calculates the triangular root of a number.
#
# Args:
#       x: A number or vector of numbers.
#      dx: A number or vector of numbers representing the uncertainty on x.
#
# Returns:
#   A data frame with columns x and dx which can be used for further
#   operations.
uadd <- function(a, b) {
  z <- a$x + b$x
  dz <- sqrt(a$dx^2 + b$dx^2)
  unum(z, dz)
}
usub <- function(a, b) {
  z <- a$x - b$x
  dz <- sqrt(a$dx^2 + b$dx^2)
  unum(z, dz)
}
umul <- function(a, b) {
  z <- a$x * b$x
  dz <- z * sqrt((a$dx/a$x)^2 + (b$dx/b$x)^2)
  unum(z, dz)
}
udiv <- function(a, b) {
  z <- a$x / b$x
  dz <- z * sqrt((a$dx/a$x)^2 + (b$dx/b$x)^2)
  unum(z, dz)
}

normalise <- function(x, FUN = sum) {
  # Returns a vector normalised by the function FUN, default being sum so the
  # vector would now sum to 1. Another example would be max, so the largest
  # value in x becomes 1.
  #
  # Args:
  #       x: A vector.
  #     FUN: A function which returns a single value when applied to a vector.
  #
  # Returns:
  #   A vector, normalised appropriately.
  if(!is.function(FUN)) stop('Passed FUN is not a function')
  x / FUN(x)
}

################################################################################
###  STATISTICS  ###############################################################
################################################################################

stdErr <- function(x) { sqrt(var(x)/length(x)) }
# For a vector x, returns the standard error on the mean.
#
# Args:
#      x: A vector.
#
# Returns:
#      The standard error on the mean.

cv <- function(x) { sd(x)/mean(x) }
# For a vector x, returns the coefficient of variation.
#
# Args:
#      x: A vector.
#
# Returns:
#      The coefficient of variation.

covar <- function(x) {
  # Wrapper function which returns the variance for a single-column vector and
  # a covariance matrix for a multi-column vector.
  #
  # Args:
  #      x: Some data.
  #
  # Returns:
  #      The covariance matrix.
  if(is.null(dim(x))) {
    return(var(x))
  } else {
    return(cov(x))
  }
}

popvar <- function(x, na.rm = FALSE) {
  # Calculates population variance instead of sample variance (which is the
  # default of the var() function in R).
  # 
  # Args:
  #      x: a vector of the population data.
  #  na.rm: a logical value indicating whether NA values should be stripped
  #         before the computation proceeds.
  #
  # Returns:
  #      The population variance.
  if(na.rm) {
    x   <- x[!is.na(x)]
  } else if(any(is.na(x))) {
    return(NA)
  }
  mean((x-mean(x))^2)
}

weightedMeanPlus <- function(x, w, na.rm = FALSE) {
  # Compute a weighted mean, where the na.rm argument ignores NA values in both
  # the values and their weights. (The default R function returns NA if any
  # weight is NA even with na.rm = TRUE.)
  # 
  # Args:
  #      x: an object containing the values whose weighted mean is to be
  #         computed.
  #      w: a numerical vector of weights the same length as x giving the
  #         weights to use for elements of x.
  #  na.rm: a logical value indicating whether NA values should be stripped
  #         before the computation proceeds.
  #
  # Returns:
  #      The weighted mean.
  if(na.rm) {
    not.na <- !(is.na(x) | is.na(w))
    wm <- weighted.mean(x[not.na], w[not.na])
  } else {
    wm <- weighted.mean(x, w)
  }
  wm
}

################################################################################
###  MISCELLANEOUS  ############################################################
################################################################################

NA2val <- function(x, val = 0) {
  # Wrapper to turn NAs in an object into a value of your choice.
  #
  # Args:
  #       x: The object containing errant NA values.
  #     val: The value to replace the NAs with, default 0.
  #
  # Returns:
  #   The object with the NAs replaced appropriately.
  x[is.na(x)] <- val
  x
}

firstElement <- function(x) {
  # Function for apply-ing to lists which will return the first element of a
  # list element
  # Args:
  #       x: An object with elements.
  #
  # Returns:
  #   The first element of that object.
  x[1]
}

permute <-
  function(
    # Randomly permute (some) elements of a vector or character string to create a
    # (slightly) randomised version of it.
    #
    # Args:
    x,
    #       A vector or character string to have its contents permuted.
    frac = 1.0,
    #       The fraction of the contents to be permuted, from 0 (no permutation)
    #       to 1 (permute everything).
    n.permute = NA
    #       The number of items to permute. Defaults to being calculated from frac
    #       but can be specified manually too. Must be a multiple of 2, because
    #       elements are swapped in pairs.
    #
    # Returns:
    #       The vector or string with n.permute of its elements permuted.
  ) {
    # if frac is not a fraction, throw an error
    if(frac < 0.0 | frac > 1.0) {
      stop(paste0('frac = ', frac,'; it must be between 0 and 1.'))
    } else if(!is.na(n.permute) & n.permute %% 2 != 0) {
      stop(paste0('n.permute = ', n.permute,'; it must be divisible by 2.'))
    }
    
    # if x is a character string, make it into a vector for processing and set a
    # reminder to put it back as a string before returning
    if(class(x) == 'character') {
      x.is.string <- TRUE
      x <- strsplit(x, '')[[1]]
    } else {
      x.is.string <- FALSE
    }
    
    # if n.permute was not provided, we can now calculate it
    if(is.na(n.permute)) {
      n.permute <- round(length(x)*frac / 2) * 2 # make sure it's a multiple of 2!
    }
    
    # if n.permute is longer than the vector...
    if(n.permute > length(x)) {
      stop(paste0('n.permute = ', n.permute,', which is greater than the length ',
                  'of the string or vector provided, ', length(x)))
    }
    
    # Create a random sample for pairs of positions to swap between
    swapsies <- sample(1:length(x), n.permute)
    
    # take those swapping positions and move them around; reversing the indices in
    # the second part of the function implies that position 1 will swap with
    # position n, 2 with n-1, etc...
    x <- replace(x, swapsies, x[rev(swapsies)])
    
    # if it was a string then reassemble it before returning
    if(x.is.string) {
      x <- paste(x, collapse='')
    }
    
    x
  }

requirePlus <- function(..., install = TRUE) {
  # Simply require a number of packages in the same command, and install them if
  # not present.
  #
  # Args:
  #   packages: A vector of the names of the packages to be imported.
  #    install: Logical indicating whether missing packages should be installed.
  #
  # Returns:
  #   Nothing (though warning and error messages are displayed on failure)
  
  package.list <- c(...)
  # if the install parameter is true, install missing packages
  if(install) {
    message('Checking for missing packages...')
    packages.present <- package.list %in% rownames(installed.packages())
    if(any(!packages.present)) {
      message(
        paste('Installing missing packages',
              paste(package.list[!packages.present], collapse = ', ')
        )
      )
      install.packages(package.list[!packages.present])
    }
  }
  # loop over packages, importing them
  require.success <- unlist(
    # suppress warnings, because we'll tell the user which packages failed later
    suppressWarnings(lapply(package.list, require, character.only = TRUE))
  )
  message(
    paste('Successfully imported packages',
          paste(package.list[require.success], collapse = ', ')
    )
  )
  if(sum(!require.success) > 0) {
    warning(
      paste('Failed to import packages',
            paste(package.list[!require.success], collapse = ', ')
      )
    )
  }
}

initParallel <- function(cores = NULL) {
  # Wrapper to initialise parallel computing functionality.
  #
  # Args:
  #   cores: The number of cores to use simultaneously. If absent, use the
  #          default from registerDoMC, 'approximately half the number of
  #          cores detected by the parallel package'.
  #
  # Returns:
  #   Nothing.
  require(doMC)
  registerDoMC(cores)
  require(foreach)
}

inRange <- function(x, rang, incl.end = rep(FALSE, length(rang))) {
  # Returns a vector of booleans specifying whether values of x fall within the
  # range rang.
  #
  # Args:
  #          x: A vector of numbers.
  #       rang: A vector specifying the range within which they are permitted to
  #             fall. It can be of any length; only the smallest and largest
  #             values are used. Called 'rang' so as not to clash with the
  #             'range' function in base R.
  #   incl.end: A vector of the same length as rang specifying whether a given
  #             endpoint is included or excluded from the range.
  #
  # Returns:
  #   A boolean with the same length of x, TRUE if within the range specified,
  #   FALSE otherwise.
  
  # find the indices of the smallest and largest values of the vector
  # (presumably usually there will only be two!)
  i.min <- which.min(rang)
  i.max <- which.max(rang)
  
  # find those which are bigger than the minimum value (including endpoint if
  # specified)...
  if(incl.end[i.min]){x >= rang[i.min]}else{x > rang[i.min]} &
    # ...and those smaller than the max...
    if(incl.end[i.max]){x <= rang[i.max]}else{x < rang[i.max]}
  # ...and return it!
}

logfileStart <- function(filename = default.logfile.name) {
  # Wrapper for creating a new blank log file during script execution.
  # NB This will silently overwrite existing files!
  #
  # Args:
  #  filename: The name of the file to create.
  #
  # Returns:
  #   Nothing.
  #
  # Globals:
  #   Creates a global variable called logfileName so that related functions
  #   know where to write to.
  logfileName <<- filename
  cat('', file = filename)
}

logfileCat <- function(...,
                       newline = TRUE, sep = "", fill = FALSE,
                       filename = logfileName
) {
  # Wrapper for adding an entry to a log file.
  #
  # Args:
  #      ... : Stuff to write to the file
  #   newline: Whether to start a new line after the entry.
  #       sep: Separator between objects to write.
  #      fill: The fill option for cat().
  #  filename: The name of the file to write to; default being the global
  #            variable set by logfileStart.
  #
  # Returns:
  #   Nothing.
  if(newline & !fill) append.me <- "\n" else append.me <- NULL
  cat(..., append.me, file = filename, sep = sep, fill = fill,
      append = TRUE)
}

logfileEnd <- function() {
  # Wrapper for blanking the existing logfileName such that no further entries
  # are written to it given the default options for logfileCat.
  #
  # Args:
  #  None.
  #
  # Returns:
  #   Nothing.
  #
  # Globals:
  #   Sets logfileName  to "".
  logfileName <<- ""
}

unixTimestamp <-function() {
  # Quick function to generate the UNIX timestamp.
  #
  # Args:
  #   None.
  #
  # Returns:
  #   Time in whole seconds since the start of the Unix epoch (01/01/1970 UTC)
  as.numeric(Sys.time())
}

getUserInput <- function(s, parse.fun = NULL, validate.fun = NULL, e = NULL) {
  # Get input from the user typing at the terminal.
  #
  # Args:
  #         s: The question to present the user with.
  # parse.fun: Optional function with which to parse the input string.
  # validate.fun: Optional function with which to validate the input string.
  #         e: Error message to display if the (cleaned) value fails validation.
  #
  # Returns:
  #   A parsed, validated input value.
  
  # loop, to keep asking for input if there's a problem
  repeat {
    # get the user's input by asking them s
    user.input <- readline(s)
    # if a function to clean input has been specified, run it
    if(!is.null(parse.fun)) {
      user.input <- parse.fun(user.input)
    }
    # if a function to validate input has been specified
    if(!is.null(validate.fun)) {
      # if the input validates...
      if(validate.fun(user.input)) {
        # ...the pass it back
        return(user.input)
      }
    } else {
      # if there's no validation to perform, return it anyway
      return(user.input)
    }
    # if we've got this far, validation must have failed...print an error
    # message and have another try...
    cat(e)
  }
}

getUserInputInteger <- function(s) {
  # Wrapper function to use getUserInput to acquire an integer from the user.
  #
  # Args:
  #         s: The question to present the user with.
  #
  # Returns:
  #   An integer.
  getUserInput(s,
               parse.fun = function(x){
                 suppressWarnings(as.integer(x))
               },
               validate.fun = function(x) {
                 ifelse(!is.na(x), TRUE, FALSE)
               },
               e = 'Could not parse input as an integer.')
}