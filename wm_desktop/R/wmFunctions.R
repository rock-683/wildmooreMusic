

#### Create Functions ####
# =================================================-

# ReactiveBindingObserver (rbo)
# This function will allow us to hold the previous 'active_slick'/carousel index value 
# after it returns to NULL, until there is a non NULL value to replace it

rbo <- function(r) {
  makeReactiveBinding("val")
  observe({
    new_value <- r()
    if (!is.null(new_value)) {
      val <<- new_value
    }
  }, priority = 10)
  reactive(val)
}

# updateValueOnEventChange (uv)
# This function will allow us to hold the previous active_slick/carousel index value
# after it returns to NULL; But also reset the output value/index 
# back to 1, when a new active_slick/carousel slide is clicked.

previous_clicked_value <- NULL

uv <- function(r, input_y) {
  makeReactiveBinding("val")
  observe({
    new_clicked_value <- r()
    if (!is.null(new_clicked_value)) {
      val <<- 1
    } else {
      val <<- input_y()
    }
  })
  
  observe({
    new_clicked_value <- input_y()
    if (!identical(new_clicked_value, previous_clicked_value)) {
      val <<- new_clicked_value
      previous_clicked_value <<- new_clicked_value
    } else {
      val <<- previous_clicked_value
    }
  }, priority = 10)
  
  reactive(val)
}

# initial populate (inpop) 
# this allows us to set the index when the page opens, 
# before a reactive element is populated with a value 


inpop <- function(input_x, val) {
  if (is.null(input_x)) {
    val = val
  } else if (!is.null(input_x)) {
    val = input_x
  }
  val
}


#' Shiny Link - Created by davidruvolo51
#'
#' Create a link to an internal tab panel
#'
#' @param to page to navigate to
#' @param label text that describes the link
#'
#' @importFrom shiny tags
#' @export


shinyLink <- function(to, label) {
  tags$a(
    class = "shiny__link",
    href = to,
    label
  )
}

