# val <- reactiveValues(); output$myPlot <- renderPlot( some function of val$bla ) !!!!! val에 그림을 저장못함 당연하지 변수저장해 이변수를 넣어 renderPlot 안에서 그려라 !  
# reactive는 실행X,  global 변수저장용 observe는 입력변화있으면 () 실행(컴용어로 side effect 부가작용 유발용, 하나의 일을 하면서 두개의 일을 하는 것) 
# reactiveValues() 변수저장용     observeEvent , eventReactive 아래
# ui<-fluidPage(actionButton("runif", "uniform"), plotOutput("plot")) #input$runif, output=list()생성
# server<-function(input, output){
# v<-reactiveValues();     observeEvent(input$runif,    {v$data<-runif(100)}     )
# output$plot <- renderPlot({if(is.null(v$data)) return(); hist(v$data)}) }
# shinyApp(ui, server)
# shiny::shinyApp(ui, server) # shinyApp(folder 위치)   runApp(folder 위치)

# ---ui 
# uiOutput("uibio") 
# --server      # observeEvent는 실시간 계속 과부하걸릴 가능성 
# vals <- reactiveValues()
# er <- eventReactive(input$b10, {    #  er <- eventReactive(input$b10, { input$in10  })로 event있을때만 변수 저장 그외는 다 실시간임!!  er()로 인풋정보 빼서 씀 out$x변수로 renderUI({ out…})담아서 보내기도
# input$in10})
# output$outbio <- renderText({tar <- er()   .....  vals$outbio <-...}) # val로담아놔야 처음부터 보이는 걸 방지   
# output$outbioui <- renderDataTable({vals$outbio}, server = T) # table click -< row number variable 
# output$uibio <- renderUI({if (is.null(vals$outbio)) return (NULL) wellPanel(dataTableOutput("outbioui"))})

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(readxl)
library(sybil)
library(sybilSBML)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(xtable)
library(foreach)
library(parallel)
library(doParallel)
library(glpkAPI)
library(rmarkdown)
library(markdown)
library(knitr)
library(dplyr)
library(DBI)
library(RMySQL)
library(DT)
library(Biobase)
library(exp2flux)
library(ComplexHeatmap)
library(circlize)
library(igraph)
library(RTCGAToolbox)
library(org.Hs.eg.db)





#
# cat("Got here", file=stderr())

# if(Sys.info()['sysname'] != "Windows"){
#   require("doMC")
#   registerDoMC(detectCores() - 1)
# }else{
#   require("doParallel")
#   cl <- makeCluster(detectCores() - 1)
#   registerDoParallel(cl)
#   #snow is also an option
# }

# warburg3 <- readSBMLmod("./warburg3.xml"); model=warburg3
# 
# 
# setGeneric("met_summary", function(object) standardGeneric("met_summary"))
# setMethod("met_summary", "modelorg", function (object) {return(data.frame(object@met_id, object@met_name, object@met_comp, object@met_de))})
# setGeneric("react_summary", function(object) standardGeneric("react_summary"))
# setMethod("react_summary", "modelorg", function (object) {return(data.frame(object@react_id,object@react_name, object@lowbnd, object@uppbnd, object@obj_coef))})
# # met=met_summary(model); react=react_summary(model)
# # printReaction(model, model, 1:length(model@react_id))
# # printObjFunc(model)
# # printReaction(model, model, which(model@obj_coef==1))

Logged = FALSE;
my_username <- "test"
my_password <- "test"


tooltipIcon <- function(text, ...,
                        link = '#', 
                        trigger = 'hover', 
                        dataplacement = "bottom") {
  tags$a(
    ...,
    # href = link,
    `data-toggle` = 'tooltip', 
    `data-animation` = 'false',
    `data-container` = 'true',
    `data-placement` = dataplacement,
    `data-trigger` = trigger,
    `class`='red-tooltip',
     title = text
  )
}



ui <- function(request) {       # request for bookmark
  tagList(
# Favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  HTML('<title>wFBASim</title>'),
  
# shinythemes::themeSelector(),
# HTML('<div class="loader"></div>'),      # http://loading.io/


#####  CDN or local ####################
# HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootswatch/3.3.7/cerulean/bootstrap.min.css">'),         #  mac : ctr + c   ===> download

HTML('<link href="maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css" rel="stylesheet">'),
# HTML('<script src="https://cdn.rawgit.com/HubSpot/tether/v1.3.4/dist/js/tether.min.js">'),

HTML('<link href="gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css" rel="stylesheet">'),

HTML('<script src="https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js"></script>'),



# HTML('https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/css/bootstrap.css'),
# HTML('https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.6/js/bootstrap.js'),
HTML('<link href=bootstrap.min.css rel=stylesheet>'), 

HTML('<link href="editor.css" type="text/css" rel="stylesheet"/>'), 
HTML('<script src="editor.js"></script>'), 
  

# HTML('<link href="...">'),
# HTML('<script src=""></script>'),


######  CSS ##########################
HTML('<style type ="text/css">
/* body background...  */
/* body, 
#selector, 
     .container, 
     .navbar-inner {
     background-color: #FDFDFF !important;
     }  */
/*   .navbar-static-top {
     position: static;
     margin-bottom: 0px;
     background-color: #FDFDFF !important;
     }    */



/* http://www.cssportal.com/examples/image-shadow.php   */

/* box-shadow...  */
    div.polaroid {
     max-width: 100%;
     width: auto;
     height: auto;
     right: auto;
     left: auto;
     background-color: white;
     box-shadow: 0 8px 16px 0 rgba(0, 0, 0, 0.2), 0 12px 40px 0 rgba(0, 0, 0, 0.19);
     text-align: center;
     margin-bottom: 10px;
     }
     div.containerrr {
     }

/* img {
  border: 2px black;
  padding: 4px;
  display: block;
  margin: auto;
  max-width: 100%;
  width :auto; 
  height: auto;
  right :ato
  left :auto
     }
*/

.imgborder {
   display:inline-block;
     position:relative;
     border:0.7px solid #ccc;
     padding:4px;
     background:#f2f2f2;
}

.s9:before, .s9:after {
   z-index: -1;
position: absolute;
content: "";
bottom: 15px;
left: 10px;
width: 50%;
top: 80%;
max-width:300px;
background: #777;
-webkit-box-shadow: 0 15px 10px #777;
-moz-box-shadow: 0 15px 10px #777;
box-shadow: 0 15px 10px #777;
-webkit-transform: rotate(-3deg);
-moz-transform: rotate(-3deg);
-o-transform: rotate(-3deg);
-ms-transform: rotate(-3deg);
transform: rotate(-3deg);
}
.s9:after {
-webkit-transform: rotate(3deg);
-moz-transform: rotate(3deg);
-o-transform: rotate(3deg);
-ms-transform: rotate(3deg);
transform: rotate(3deg);
right: 10px;
left: auto;
}




*{
  font-family: :"Lato";
     }
     
     #profile-grid { overflow: auto; white-space: normal; }
     #profile-grid .profile { padding-bottom: 40px; }
     #profile-grid .panel { padding: 0 }
     #profile-grid .panel-body { padding: 15px }
     #profile-grid .profile-name { font-weight: bold; }
     #profile-grid .thumbnail {margin-bottom:6px;}
     #profile-grid .panel-thumbnail { overflow: hidden; }
     #profile-grid .img-rounded { border-radius: 4px 4px 0 0;}




/* left color bar...  */
.bs-callout {
    padding: 20px;
     margin: 20px 0;
     border: 1px solid #eee;
     border-left-width: 5px;
     border-radius: 3px;
     }
     .bs-callout h4 {
     margin-top: 0;
     margin-bottom: 5px;
     }
     .bs-callout p:last-child {
     margin-bottom: 0;
     }
     .bs-callout code {
     border-radius: 3px;
     }
     .bs-callout+.bs-callout {
     margin-top: -5px;
     }
     .bs-callout-default {
     border-left-color: #777;
     }
     .bs-callout-default h4 {
     color: #777;
     }
     .bs-callout-primary {
     border-left-color: #428bca;
     }
     .bs-callout-primary h4 {
     color: #428bca;
     }
     .bs-callout-success {
     border-left-color: #5cb85c;
     }
     .bs-callout-success h4 {
     color: #5cb85c;
     }
     .bs-callout-danger {
     border-left-color: #d9534f;
     }
     .bs-callout-danger h4 {
     color: #d9534f;
     }
     .bs-callout-warning {
     border-left-color: #f0ad4e;
     }
     .bs-callout-warning h4 {
     color: #f0ad4e;
     }
     .bs-callout-info {
     border-left-color: #5bc0de;
     }
     .bs-callout-info h4 {
     color: #5bc0de;
     }



/* .loader {
	position: fixed;
     left: 0px;
     top: 0px;
     width: 100%;
     height: 100%;
     z-index: 9999;
     background: url("page-loader.gif") 50% 50% no-repeat rgb(249,249,249);
     }     */


/*
.carousel-inner > .item > img,
.carousel-inner > .item > a > img {
max-height: 65px;    /* Set slide height here */
 }
*/


/*  html,body {
  height:65px;
background-color:#ffffff;
}    */


/*   .carousel-inner,.carousel,.item,.container,.fill {
  height:65px;
     width:100%;
     background-position:center center;
     }   */

/*  .navbar-brand {
  padding: 0px;
}
.navbar-brand>img {
  height: 100%;
  padding: 15px;
  width: auto;
}  */


/*  navbar!!!!!!!  */ 
#backgroundImg
{
  color: #ff0000;
  background-color: #000000;
  height: 20px
  }
  .navbar {
  margin-bottom: 10px !important;
  }


/*  if you wnat to change navbar s height 
.navbar-brand {
    padding-top:1px !important; padding-bottom:0 !important;
     height: 15px;
     }    */ 


/*
.navbar {min-height:30px !important;} 
*/
 
/*
.navbar .navbar-nav {float: left}
 .navbar .navbar-header {float: left} 

*/ 
 

.mymodal-body{position:relative;padding:20px}




</style>'),



HTML('<script> 
     $(function(){ $("h3").html("jquery test"); });
     </script>'),      



HTML('<script> $(document).ready(function() {
/* show lightbox when clicking a thumbnail */
     $("a.thumb").click(function(event){
     event.preventDefault();
     var content = $(".modal-body");
     content.empty();
     var title = $(this).attr("title");
     $(".modal-title").html(title);
     content.html($(this).html());
     $(".modal-profile").modal({show:true});
     });

     }); </script>'),



# I think the most correct answer, assuming the use of jQuery, is a consolidation of aspects of all the answers in this page, plus the use of the event that Bootstrap passes: It also would work changing $(document) to $('.modal') or to add a class to the modal that signals that this focus should occur, like $('.modal.focus-on-first-input')!!!
HTML('<script> $(document).on("shown.bs.modal", function(e) {
  $("input:visible:enabled:first", e.target).focus();
});</script>'),

HTML('<script type="text/javascript">
$(document).ready( function() {
  $("#txtEditor").Editor(); 
});</script>'),
  



# 
# HTML('<script> $(document).ready(function() {
#      $("#loginForm").formValidation({
#      framework: "bootstrap",
#      excluded: ":disabled",
#      icon: {
#        valid: "glyphicon glyphicon-ok",
#        invalid: "glyphicon glyphicon-remove",
#        validating: "glyphicon glyphicon-refresh"
#      },
#      fields: {
#        username: {
#          validators: {
#            notEmpty: {
#              message: "The username is required"
#            }
#          }
#        },
#        password: {
#          validators: {
#            notEmpty: {
#              message: "The password is required"
#            }
#          }
#        }
#      }
#      });
# });
#   </script>'),




# HTML('
# <div id="myCarousel" class="carousel slide">
#      <!-- Indicators -->
#      <ol class="carousel-indicators">
#      <li data-target="#myCarousel" data-slide-to="0" class="active"></li>
#      <li data-target="#myCarousel" data-slide-to="1"></li>
#      <li data-target="#myCarousel" data-slide-to="2"></li>
#      </ol>
#      
#      <!-- Wrapper for slides -->
#      <div class="carousel-inner">
#      <div class="item active">
#      <div class="fill" style="background-image:url("http://placehold.it/1900x1080&amp;text=Slide One");"></div>
#      <div class="carousel-caption">
#      <h1>Modern Business - A Bootstrap 3 Template</h1>
#      </div>
#      </div>
#      <div class="item">
#      <div class="fill" style="background-image:url("http://placehold.it/1900x1080&amp;text=Slide Two");"></div>
#      <div class="carousel-caption">
#      <h1>Ready to Style &amp; Add Content</h1>
#      </div>
#      </div>
#      <div class="item">
#      <div class="fill" style="background-image:url("http://placehold.it/1900x1080&amp;text=Slide Three");"></div>
#      <div class="carousel-caption">
#      <h1>Additional Layout Options at <a href="http://startbootstrap.com">http://startbootstrap.com</a></h1>
#      </div>
#      </div>
#      </div>
#      
#      <!-- Controls -->
#      <a class="left carousel-control" href="#myCarousel" data-slide="prev">
#      <span class="icon-prev"></span>
#      </a>
#      <a class="right carousel-control" href="#myCarousel" data-slide="next">
#      <span class="icon-next"></span>
#      </a>
#      </div>
#      '),




# # internal tab href : 


# actionButton("goo","Submitt", onclick ="location.href='#plotttt'", class = "btn btn-success btn-xs"),
# actionButton("goo","Submitt", onclick ="window.open('http://220.85.17.8:3838/wFBA', '_self')", class = "btn-primary"),
# actionButton("goo","Submitt", onclick ="window.open('http://220.85.17.8:3838/wFBA', '_blank')", class = "btn-primary"),

# HTML('<script> $(function(){ $("h3").html("jquery test"); }); </script>'), 

# HTML('<script type="text/javascript">
#      $(window).load(function() {
#      $(".loader").fadeOut("fast");
#      })
#      </script>'),



# bootstrapPage(
#   titlePanel("Carousel Example"),
#   carousel("caro", slide1, slide2, slide3)
# ),

# 
# HTML('<!-- Single button -->
# <div class="btn-group">
#      <button type="button" class="btn btn-success dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
#      Action <span class="caret"></span>
#      </button>
#      <ul class="dropdown-menu">
#      <li><a href="#tabbb">Action</a></li>
#      <li><a href="#">Another action</a></li>
#      <li><a href="#">Something else here</a></li>
#      <li role="separator" class="divider"></li>
#      <li><a href="#tab_reactend"  data-toggle="tab" >Separated link</a></li>
#      </ul>
#      </div>'),

# internal tab href :  href=""http://210.119.218.27/#tab_XXXXXX Go to Specific Tab  but full page reload...not good..

# modified narvar : http://bootsnipp.com/snippets/featured/toggle-navbar-with-slide-down-animation

# BS exercise : https://www.w3schools.com/bootstrap/bootstrap_tabs_pills.asp

#  icon :  Font Awesome and Glyphicons" librarie  https://shiny.rstudio.com/reference/shiny/latest/icon.html

# HTML colors    : http://www.immigration-usa.com/html_colors.html

# board   :   stackExchange에 community 개설  http://askubuntu.com/questions/397502/reboot-a-server-from-command-line 
# The helper functions that can call their equivalent tags without using the tag syntex (tags$) are: a, br, code, div, em, h1, h2, h3, h4, h5, h6, hr, img, p, pre, span, and strong.  

# top header HTML  https://datatables.net/manual/index#DataTables-Manual

# BS plugin color   :  http://labs.abeautifulsite.net/jquery-minicolors/

# BS best tutorial :        https://v4-alpha.getbootstrap.com/content/tables/
# BS best tutorial :        https://startbootstrap.com/bootstrap-resources/

# BS login : http://bootsnipp.com/snippets/featured/login-amp-password-reminder

# A WYSIWYG Editor built for Bootstrap   : http://innovastudio.com/BootstrapLiveEditor/

# http://formvalidation.io/examples/cant-submit-form-after-validation/

# Shiny Database App (CRUD)  : https://ipub.com/shiny-crud-app/

# Basic shiny syntax : req(x) = validate(need(x, message = FALSE))
# Observe Similar to reactive functions but they are not a function and thus have no return output.      
# Reactive expression : http://www.alshum.com/shiny-reactive/
# http://stackoverflow.com/questions/32821337/shiny-observe-event-executing-function-as-reactive .....it could be to use observe() inside observerEvent(). Indeed, each time that the values of the intern code changes, the observe() will execute everything inside in it.
# testReactive<-reactive({as.numeric(input$numberTest)})  #reactive=function=variable for later use...
# observeEvent(input$bTest,{
#   observe({   
#     x<-10+testReactive()
#     output$testing<-renderText(print(x))#     
#   }) #Closed observe
# }) #Closed observeEvent





#############################################################

            # navbarPage( p(span(a(href="google.com","centrOmics", style="text-decoration: none; color : #D0E6FF "), style ="font-size:17pt; color: sky blue; font-weight: normal; font-style: normal; font-family :'helvetica'"), span("[ FluxCore ]", style ="font-size:15pt; font-style: normal; font-family :'helvetica'")), theme = shinytheme("cerulean"),
            #             position = "static-top",
            #             responsive = NULL,
            #             # icon = icon("bar-chart-o"),
            #             fluid = T,
            #             inverse = F,
            #             collapsible = T,
            #             # HTML('<button type="button" class="btn btn-default navbar-btn">Sign in</button>'),

            #               tabPanel("Description", id="tabbb" , 
                                 


fluidPage(
  HTML(
    '
    
    <nav class="navbar navbar-default">
    <div class="container-fluid">
    <div class="navbar-header">
    <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
    <span class="sr-only">Toggle navigation</span>
    <span class="icon-bar"></span>
    <span class="icon-bar"></span>
    <span class="icon-bar"></span>
    </button>
    
    
    <a class="navbar-brand" href="#"> <bold> &nbsp; &nbsp; &nbsp; OncoFlux  &nbsp;  &nbsp;  &nbsp; </bold>
    <!--    <img alt="Brand" src="11.png">  -->
    </a>
    </div>
    
    
    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
    <ul class="nav navbar-nav">
    <li class="active"><a href="#">wFBASim</a></li>
    <li><a href="http://220.85.17.8:3838/wFBA/#top">eFluxSim</a></li>
  <!---  <li><a href="#">Page 2</a></li>
    <li class="dropdown"> 
    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
    <ul class="dropdown-menu">
    <li><a href="#">Action</a></li>
    <li><a href="#">Another action</a></li>
    <li><a href="#">Something else here</a></li>
    <li role="separator" class="divider"></li>
    <li><a href="#">Separated link</a></li>
    <li role="separator" class="divider"></li>
    <li><a href="#">One more separated link</a></li>
    </ul>
    </li>   --->
    
    </ul>
    
    <!-----  <form class="navbar-form navbar-left">
    <div class="form-group">
    <input type="text" class="form-control" placeholder="Search">
    </div>
    
    <button type="button" class="btn btn-default">Submit</button>
    </form>        ------->
    
    
    
    <ul class="nav navbar-nav navbar-right">

<!--   <?php if(global $user):?>   -->

<li id="outout" style = "display:none;"> <a href="http://220.85.17.8:3838/wFBA">Sign Out </a></li>

 <li id="loglog"><a href="#" id="show" type="button" class="btn btn-link action-button shiny-bound-input" ><span class="glyphicon glyphicon-log-in"></span> Sign In </a></li> 




<!--  
    <li class="dropdown">
    <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
    <ul class="dropdown-menu">
    <li><a href="#">Action</a></li>
    <li><a href="#">Another action</a></li>
    <li><a href="#">Something else here</a></li>
    <li role="separator" class="divider"></li>
    <li><a href="#">Separated link</a></li>
    </ul>
    </li>
    <li><a href="#"><span class="glyphicon glyphicon-user"></span> Sign Up </a></li>
    <li><button id="show" type="button" class="btn btn-default action-button shiny-bound-input btn-block">Show modal dialog</button></li> 
    <li><a href="#login-overlay" data-toggle="modal" data-target="#login-overlay" data-dismiss="modal" ><span class="glyphicon glyphicon-log-in"></span> Login </a></li> 
                 -->
 </ul>
    </div>
    </div>
    </nav>



    

 <!-- Modal -->




<div class="modal fade" id="login-overlay" tabindex="-1" role="dialog" aria-labelledby="Login" aria-hidden="true" >
    <div class="modal-dialog" role="document">
    
    <!--     <div id="login-overlay" class=" modal fade modal-dialog">   -->
    <div class="modal-content">
    <div class="modal-header">
    <button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">×</span><span class="sr-only">Close</span></button>
    <h4 class="modal-title" id="myModalLabel">Login to site.com</h4>
    </div>
    <div class="mymodal-body"> 
    <div class="row">
    <div class="col-xs-6">
    <div class="well">
    <form  id="loginForm" method="post" accept-charset="UTF-8">
    <div class="form-group">
    <label class="sr-only" for="exampleInputEmail2">Email address</label>
 <input type="email" class="form-control" name="username" placeholder="Email address" required>
    <span class="help-block with-errors"></span>
    </div>
    <div class="form-group">
    <label for="password" class="control-label">Password</label>
    <input type="password" data-minlength="6" class="form-control" id="password" name="password" required>
    <span class="help-block with-errors">Minimum of 6 characters</span>
    </div>
    <div id="loginErrorMsg" class="alert alert-error hide">Wrong username or password</div>
    <div class="checkbox">
    <label>
    <input type="checkbox" name="remember" id="remember"> Remember login
    </label>
    <p class="help-block">(if this is a private computer)</p>
    </div>

     <div class="form-group">  
    <!--  add action-button for shiny!!!!!!!  input$Login -->
 <!---   <button id="show01" type="button" class="btn btn-default action-button shiny-bound-input btn-block">Show modal dialog</button>   --->
    <button type="button" id ="Login" value="login" name="submit form" class="btn btn-success action-button shiny-bound-input btn-block"  >Login</button>
    <button type="button" class="btn btn-default" data-dismiss="modal">Cancel</button>
    <a href="/forgot/" class="btn btn-default">Help to login</a>
    </div>
    </form>


    </div>
    </div>
    <div class="col-xs-6">
    <p class="lead">Register now for <span class="text-success">FREE</span></p>
    <ul class="list-unstyled" style="line-height: 2">
    <li><span class="fa fa-check text-success"></span> See all your orders</li>
    <li><span class="fa fa-check text-success"></span> Fast re-order</li>
    <li><span class="fa fa-check text-success"></span> Save your favorites</li>
    <li><span class="fa fa-check text-success"></span> Fast checkout</li>
    <li><span class="fa fa-check text-success"></span> Get a gift <small>(only new customers)</small></li>
    <li><a href="/read-more/"><u>Read more</u></a></li>
    </ul>
    <p><a href="/new-customer/" class="btn btn-info btn-block">Yes please, register now!</a></p>
    </div>
    </div>
    </div>  
    </div>
    </div>  
    </div>  



    
    '),
  
  # tags$style(type="text/css", ".selectize-input{ z-index: 0; }"),
  # titlePanel("2017 Biochem wFBASim"),
  p(
    span("Biochem practice : wFBASim", style = "font-size:20pt; font-style: normal; font-family :'Helvetica'"),
    HTML('&nbsp;&nbsp;'),
    "Department of Biochemistry, School of Medicine, Konkuk Univ.",
    a("palelamp@gmail.com", href = "mailto:palelamp@gmail.com"),
    style = "font-size:10 pt ; font-style: italic; font-family: 'times';"
  ),
  # style="font-size:20pt"  "color:blue"
  # a("Sung Young Kim, MD, PhD", href = "mailto:palelamp@gmail.com"),


  sidebarLayout(
    sidebarPanel(width = 3,
      shinyjs::useShinyjs(),
      id = "side-panel",
      
      div(uiOutput("bookmark", inline = T), actionButton("gooo", "Reset", class = "btn btn-success btn-xs", onclick = "window.open('http://220.85.17.8:3838/wFBA', '_self')")),
      uiOutput("hline", inline = T), 
     
      div( id= "uploadcheckbox",
      checkboxInput('uploadcheck', 'Upload local data', F)),
      
      div( id= "uploadtoggle",
      selectizeInput(
        "demo", "Demo:", choices = list( "human core metabolism (HCCN)" = "warburg3",
                                         "HCCN with Crowding Constraints" = "warburg_crowd"), 
        options = list(
          placeholder = 'Select SBML below',
          onInitialize = I('function() { this.setValue(""); }')
        ) 
      ),
      hr(),
      fileInput('txtInput', "Choose XML/SBML file to upload", accept = c(
        '.xml','.sbml')), 
      
      # dataTableOutput('mytable'),
      fileInput('file', 'Choose Expr file to upload',
                  accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  '.xls',
                  '.xlsx'
                  
                  
                )
      ),
     
      # selectInput("demo", "Demo:",
      #             c("Warburg Core" = "warburg3",
      #               "Warburg Crowding Constraints" = "warburg_crowd"), selectize = F),
      
     # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
    
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      ################################################################
      
      actionButton("choice", "incorporate external info"),
      hr(),
      selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
      tableOutput("table_display")),
      
      
      # 정규표현식 : https://wiki.kldp.org/HOWTO/html/Adv-Bash-Scr-HOWTO/x10468.html
      
     
      
        div(
        shinyjs::useShinyjs(),
        id = "reset" ,
        # h4(textOutput("upload")),
        hr(),
        uiOutput("Flux"),
        uiOutput("bound"),
        uiOutput("bound01"),
        uiOutput("addreact"),
        uiOutput("addex"),
       # uiOutput("login"),
       
        hr(),
        fluidRow(column(6,
                        uiOutput("printreact")),
                 column(6, uiOutput("go"))),
        uiOutput("hline1"),
        #downloadButton('downloadData', 'Download Data'),
        radioButtons('format', 'Document format', c('HTML', 'Word', 'PDF'),
                     inline = TRUE),
        downloadButton('downloadReport', 'Download Report', class = "btn btn-primary btn-block"),
       # downloadButton('reportpdf'),
        
        # actionButton("goo", "Submitt", onclick =
        #                "location.href='#plotttt'", class = "btn btn-success btn-md", title="tooltip"),
        # tooltipIcon('test', icon('question-circle')),
      hr(),
        div( id= "gprbuttonshow", style = "display:none", actionButton("gpr", "GPR", class = "btn btn-success btn-xs disabled", icon("forward")) )    # style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        
        
      ),
      tags$div(class = "dd")
    ),
    
    
    mainPanel(id = "test1", width =9,
              
              tags$div(
                tabsetPanel(
                  id = "inTabset",
         ##########################################################         
                  tabPanel(
                    "Description",
                    withMathJax(),
                    
                    # HTML("intro text <a href='#distPlot2'>Go to plot 2</a> intro text "),
                    # tags$blockquote(
                    #   h5(
                    #     "Tidy data sets are all the same. Each messy data set is messy in its own way.",
                    #     cite = "Hadley Wickham"
                    #   )
                    # ),
                    # tags$code("This text will be displayed as computer code."),
                    # tags$em("This text is emphasized."),
                    # tags$i("This text is italicized."),
                    # tags$ol(
                    #   tags$li("First list item"),
                    #   tags$li("Second list item"),
                    #   tags$li("Third list item")
                    # ),
                    # tags$pre("This text is preformatted."),
                    # tags$div(HTML(paste(
                    #   "E = mc", tags$sup(2), sep = ""
                    # )),
                    # HTML(paste(
                    #   "H", tags$sub(2), "0", sep = ""
                    # ))),                 
                    HTML('<div class="page-header">
	<h1>Flux Balance Analysis<small><i> simplifying the cell metabolic complexity</i></small></h1></div>'),                               
                                    withMathJax(),
                    p('The basic strategy of FBA is to identify steady state metabolic rates (fluxes) that satisfy a set of constraints, and maximize (or minimize) a given objective function. The fundamental principle in FBA is the conservation of mass. A flux balance can be written for each metabolite (Xi) within a metabolic system to yield the dynamic mass balance equations that interconnect the various metabolites. Equating the rate of accumulation of Xi to its net rate of production, the dynamic mass balance for Xi is: '),
                    
                   # p('Steady-state assumption: internal metabolite concentrations are constant over time,'),
                                   # p("$$\\frac{\\mathrm{d} X }{\\mathrm{d} t} = 0$$"),
                   p("$$\\frac{\\mathrm{dX_i} }{\\mathrm{d} x} = V_{syn} -V_{deg} -(V_{use} - V_{trans})$$"),
                   
              p('where the subscripts, syn and deg refer to the metabolic synthesis and degradation of metabolite Xi. The uptake or secretion flux, Vtrans and Vuse can be determined experimentally. therefore, the above equation can be written as: (bi is the net transport out of our
                defined metabolic system)'),     
                   
                   p("$$\\frac{\\mathrm{dX_i} }{\\mathrm{d} x} = V_{syn} -V_{deg} -b_i$$"),
                   
                   
                   p('The stoichiometric matrix (S matrix), and its dot product with the flux vector (v) represents the mass balance constraint :'),
                    
#  p("$$\\begin{bmatrix}
# -1 & -1 &  1&  0& 0\\\\ 
#  1&  0&  0&  -1&0 \\\\ 
#  0&  1&  0&  0& -1
# \\end{bmatrix} \\begin{bmatrix}
# v_1\\\\ 
# v_2\\\\ 
# b_1\\\\ 
# b_2\\\\ 
# b_3
# \\end{bmatrix} =\\begin{bmatrix}
# 0\\\\ 
# 0\\\\
# 0
# \\end{bmatrix}$$"),

              
                    p("$$\\sum_{j=1}^{n}S_{ij}v_j=b_i   \\:\\:\\:\\:\\:\\:  \\forall i=1,...m $$"),
                    p('where S is the m (metabolites) by n (reactions) stoichiometric matrix, v is the vector of metabolic fluxes. For a metabolic network all the transient material balances can be represented by a single matrix equation:'),
             p("$$\\frac{\\mathrm{dX} }{\\mathrm{d} x} = S\\cdot v-b$$"),


# HTML('<center><div ><img src="fba.png" class="img-thumbnail" alt="Responsive image" ></div></center>'),  # style="width:300px"


              
     h4("Objective function"),                             
                    p('The flux-balance equation is typically under-determined (m < n), and
cannot be solved using Gaussian elimination. The determination of a particular metabolic flux distribution has been formulated as a linear programming problem. The idea is to maximize an objective function Z that is
                    subject to the stoichiometric and capacity constraints where Cj represents weights for the individual rates :'), 
tags$blockquote( p("$$f(v)= \\sum_{j=1}^{n}c_k v_k = c^Tv \\rightarrow max | min $$")),
                    p('The objective function is the linear combination where v = [v1, v2, …, vN] is the flux vector, and c is the vector of coefficients of the linear combination. The dimension N of both vectors is equal to the number of reactions (i.e. metabolic fluxes) in the model. In the case of biomass (objective function) maximization, vector c is an all-zero vector except for a one (1.0) in the position corresponding to the biomass(obj) reaction:'),
                                                      p("$$ f_{obj}(v)=c^Tv =\\begin{bmatrix}
1 & 0 & 0 &  ...& 
\\end{bmatrix} \\begin{bmatrix}
v_{obj}\\\\ 
v_1\\\\ 
v_2\\\\ 
...\\\\
\\end{bmatrix} $$"),

                  
# p('vLB is a vector of lower bounds for all fluxes, vUB is a vector of upper bounds for all fluxes, b is the vector of the rates of accumulation/depletion of each metabolite, and c is the vector defining the contribution of different fluxes to the objective function.'), 


                      p('A canonical FBA calculation can be expressed as the following LP problem:'),
                  tags$ol(tags$li('Maximize', span("$$c^Tv$$")),
                     tags$li('Subject to', span("$$S\\cdot v=0$$")  ),
                     tags$li('and', span("$$lower bound \\leq v \\leq upper bound$$") ) ) ,
                                                   
                    
                    
                    br(),
                    br(),
                    
                    HTML('<div class="page-header">
	<h1>Flux Balance Analysis <small> <i>with macromolecular crowding</i></small></h1></div>
<p>Constraint-based metabolic models have proven to be particularly useful as they enable genome-scale modeling of metabolism. The approach based on macromolecular crowd constraints have enabled quantitative modeling of metabolic processes and played key roles in guiding scientific research. Enzymes carrying out energy-production pathways can at most occupy a fraction of the intracellular volume.(e.g. the mitochondria). Because reactions are assumed to be enzyme-restricted, the activity of aerobic pathways will necessarily level out once all deputed enzymes work at full speed, while that of glycolysis may still increase.</p>

<p> The crowding constraint is implemented in the following equation : </p>'), 

tags$blockquote(p("$$V_{gly} + V_{oxphos} + V_{LDH} \\leq V_{ATP}$$")),

HTML('<p>  where VATP represents the cell volume devoted to ATP production, whereas Vglyc, Voxphos, and VLDH are the volumes taken up by the glycolytic, OXPHOS, and LDH enzymes, respectively. ϕ ATP = V ATP /V is the total volume fraction of the cell cytoplasm occupied by glycolytic enzymes.Introducing the constants aglyc, aox, and aLDH representing the volume occupied per unit of ATP production by glycolytic, OXPHOS and LDH enzymes, respectively.</p>'),


tags$blockquote(p("$$[a_{gly} + a_{LDH}] \\:f_{LDH} + [a_{gly} + a_{LDH} ]\\: f_{oxphos}  \\leq \\Phi_{ATP}$$")), 

HTML('<p> The empirical estimates aglyc ≃ 0.0027 (min/mM), aLDH ≃ 0.000046  (min/mM), aox ≃ 0.02 (min/mM) and ΦATP = 0.4, indicating that the mitochondria contributes about 5 and 50 times more to molecular crowding than glycolytic enzymes and lactate dehydrogenase, respectively. </p>'),
                
              
              br(),
              br(),
              HTML('<div class="page-header">
	<h1>A genome-scale network reconstruction (GENRE) <small> <br><i>Optimizing genome-scale network reconstructions</i></small></h1></div>
<p>A genome-scale network reconstruction (GENRE) represent biochemically and genetically structured knowledge bases  they can be interrogated using constraint-based reconstruction. GENRE is built systematically using genome annotation, "omics" data sets and legacy knowledge. Thus, GENREs should provide the best representation of the metabolic capabilities of a target organism on the basis of the information available at the time of reconstruction. They allow researchers to test and share new hypotheses about metabolic functions in a target organism. As a result, interest in network reconstructions and the scope of their applications has grown rapidly.</p>'),

              
              
                               
                                     

HTML('
<p></p>


<p></p>


<p></p>



<p></p>'),
                    hr(),
                    
                    
                    # verbatimTextOutput("dataInfo"),
                    # 
                    # HTML('<center><div class="s9 imgborder"><img src="sss.png" class="img-thumbnail" alt="Responsive image" ></div></center>'),  # style="width:300px"
                   
#                       hr(),
#                     HTML(
#                       '<center><div class="s9 imgborder">
#                        <a href="#picmodal", class="thumb">
#                       <img src="sssss.png" class="img-thumbnail"  style="width:70%" alt="Responsive image" data-toggle="modal" data-target="#picmodal" >
#                       </a>
#                       <div class="containerrr">
#                       </div>
#                       </div></center>'
#                     ),
#                     
#               HTML('<button type="button" class="btn btn-info" data-toggle="collapse" data-target="#demo">Simple collapsible</button>
#   <div id="demo" class="collapse in">
#                    Lorem ipsum dolor sit amet, consectetur adipisicing elit,
#                    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
#                    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
#                    </div>'),
#                     
#                     
#                     
#                     
#                     HTML('<a href="#picmodal", class="thumb">
#     <img src="sssss.png" class="img-responsive img-rounded" data-toggle="modal" data-target="#picmodal"  data-dismiss="modal">
#                          </a>
#                          
#                         
# 	<div class="modal fade" id="picmodal" >
#                          <div class="modal-dialog ">
#                          <div class="modal-content">
#                          <div class="modal-header">
#                          <button class="close" type="button" data-dismiss="modal">×</button>
#                          <h3 class="modal-title"></h3>
#                          </div>
#                          <div class="modal-body">
#                          </div>
#                          <div class="modal-footer">
#                          <button class="btn btn-default" data-dismiss="modal">Close</button>
#                          </div>
#                          </div>
#                          
#                          </div>
#                          </div>
#                         
#                          
#                         
#                          
#                          '),
#                     
#                     
#                      HTML('<!-- Button trigger modal -->
# <button type="button" class="btn btn-primary btn-lg" data-toggle="modal" data-target="#myModalll">
#                          Launch demo modal
#                          </button>
#                          
#                          <!-- Modal -->
#                          <div class="modal fade" id="myModalll" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
#                          <div class="modal-dialog" role="document">
#                          <div class="modal-content">
#                          <div class="modal-header">
#                          <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
#                          <h4 class="modal-title" id="myModalLabel">Modal title</h4>
#                          </div>
#                          <div class="mymodal-body">
#                           <p> " hello " </p>
#                          </div>
#                          <div class="modal-footer">
#                          <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
#                          <button type="button" class="btn btn-primary">Save changes</button>
#                          </div>
#                          </div>
#                          </div>
#                          </div>
#                          
#                         
#                          '),
#                     
#                     
#                     
#                     
#                     hr(),
#                     h3("testtest"),
#                     hr(),
                 
                    

#                     checkboxInput("chk2", "Checkbox 2"), verbatimTextOutput("summary.end"),
#                    
#                     
#                     
#                     HTML(
#                       '<div class="bs-callout bs-callout-primary" id=callout-navs-anchor-disabled> <h4>Link functionality not impacted</h4> <p>This class will only change the <code>&lt;a&gt;</code>s appearance, not its functionality. Use custom JavaScript to disable links here</p> </div>'
#                     ),
#                     HTML(
#                       '<div class="bs-callout bs-callout-success" id=callout-navs-anchor-disabled> <h4>Link functionality not impacted</h4> <p>This class will only change the <code>&lt;a&gt;</code>s appearance, not its functionality. Use custom JavaScript to disable links here</p> </div>'
#                     ),
#                     HTML(
#                       '<div class="bs-callout bs-callout-warning" id=callout-navs-anchor-disabled> <h4>Link functionality not impacted</h4> <p>This class will only change the <code>&lt;a&gt;</code>s appearance, not its functionality. Use custom JavaScript to disable links here</p> </div>'
#                     )
#                
div(id="txtEditorr", style = "display:none",
     div( id="txtEditor" ))
  


                  ),
                 






                 tabPanel(
                    "Meta-info",
                    icon = icon("tags"),
                    style = "padding-top: 2px;",
                    shinyjs::useShinyjs(),
                    id = "reset1",
                    
                    
    
                    # conditionalPanel(
                    #   condition = "input.demo == '' && input.txtInput == NULL",
                    #   p("Please select a file to upload.")
                    # 
                    # 
                    # 
                    # ),
                    
                    
                        
                    br(),
                    div(id="testtestt", align="left",  p("Select the local/demo file to be uploaded.")),
                        
                       
                    br(),
                    DT::dataTableOutput("Smatrix"),
                    br(),
                    br(),
                    DT::dataTableOutput("reactname"),
                    br(),
                    br(),
                    DT::dataTableOutput("gpr"),
                    
                   tableOutput('testtesttest'),
                  #  is.null(input.txtInput)                    
                    
                    
                 
                  
                    # textOutput("loglog"),
                    # 
                    # numericInput("nrows", "MySQL db:lamp table:test Enter the number of rows to display:", 2),   # for mysql
                    # DT::dataTableOutput("tbl"),     # for mysql
                    # 

                    
                    
                    # plotOutput("n", width = "25%")
                   
                  
                   br(),
                    a(name = "reactstart")
                   
                                        ),
                  # navbarMenu(
                  #   "Menu",
                  #   HTML('<li><a href="http://google.com">Link</a></li>'),
                  #   HTML('<li><a href="#plotttt">Link</a></li>'),
                  #   HTML('<li><a href="#tab-jquery02">Link</a></li>')
                  # ) ,
                  
                  tabPanel(
                    "Reactions",
                    icon = icon("table"),
                    
                    br(),
                    div(id="testtest", align="left",  p("Select the local/demo file to be uploaded.")),
                    
                    DT::dataTableOutput("showreaction"),
                    
                    
                    
                    
                    textOutput("ttt"),
                    tableOutput("ttttt")
                    # HTML(
                    #   #https://v4-alpha.getbootstrap.com/utilities/responsive-helpers/
                    #   #<div class="embed-responsive embed-responsive-16by9">
                    #   #<iframe class="embed-responsive-item" src="//www.youtube.com/embed/zpOULjyy-n8?rel=0" allowfullscreen></iframe></div>
                    #   
                    #   '<iframe src="https://gference.github.io/chord/" style="border: none; width: 100%; height: 1000px" frameborder="0"></iframe>'
                    # )
                  ),
                  
                  tabPanel(align="center",
                    "Results",
                    icon = icon("bar-chart-o"),
                    
                    br(),
                    div(id="testtesttt", align="left",  p("Select the local/demo file to be uploaded.")),
                   
                    div( id= "fbashow", style = "display:none",
                    br(),
                    div(align="left",
                    plotOutput("plotttt", width="78%")),
                    
                    hr(),
                    DT::dataTableOutput("tttt")),
                   
                    
                    
                    
                    # plotOutput("igraph", width="800px", height = "800px"),
                    # 
                    # plotOutput("plotcircos", width="800px", height = "800px"),
                    # 
                    # plotOutput("plottest", width="70%"),
                    # 
                    # tableOutput("mat"),
                    # 
                    # tableOutput("fluxDiff"),
                    # 
                    # verbatimTextOutput("fluxDifff"),
                    # 
                    # DT::dataTableOutput("fluxDiffff"), 
                    
            div(id="abs",
                     absolutePanel(
                      top = 45,
                      right = 20,
                      width = 270,
                      draggable = T,
                      wellPanel(style = "opacity: 0.75", 
                                uiOutput("bound02", height = "200px"),
                                uiOutput("bound03", height = "200px"),
                                uiOutput("bound04", height = "200px"),
                                uiOutput("bound05", height = "200px"),
                                uiOutput("bound06", height = "200px"),
                                uiOutput("bound07", height = "200px"),
                                uiOutput("bound08", height = "200px"),
                                actionButton("goabs", "Submit", onclick = "location.href='#top'", class = "btn-primary btn-xs") 
                                
                                
                                
                                )
                      # style = "opacity: 0.72"
                    )),
            
            div( id= "gprshow", style = "display:none",
            br(),
            hr(),
            DT::dataTableOutput("gprexpr"),
            br(),
            hr(),
            DT::dataTableOutput("fluxDiffff"),  
            br(),
            hr(),
            plotOutput("igraph", width="800px", height = "800px")),
            
            br()
            
            
            
             
                    # HTML(
                    #   '<iframe src="http://llidesign.co.uk/iconic-furniture/#section-10" style="border: none; width: 100%; height: 1000px" frameborder="0"></iframe>'
                    # )
                    
                  ),
                  tabPanel(
                    "Download",
                    icon = icon("download"),
                    br(),
                    div(id="testtestttt", align="left",  p("Select the local/demo file to be uploaded.")),
                    a(name = "aaa"),
                    
                    radioButtons('format02', 'Document format', c('HTML', 'Word', 'PDF'),
                                 inline = TRUE),
                    downloadButton('downloadReport02', 'Download Report', class = "btn btn-primary btn-block")
                    # HTML(
                    #   '<iframe src="https://tympanus.net/Development/PageTransitions/" style="border: none; width: 100%; height: 1000px" frameborder="0"></iframe>'
                    # )
                    
                  )
                  # tabPanel(
                  #   "jquery03",
                  #   icon = icon("tags"),
                  #   br(),
                  #   HTML(
                  #     '<iframe src="https://tympanus.net/Development/PageTransitions/" style="border: none; width: 100%; height: 1000px" frameborder="0"></iframe>'
                  #   )
                  #   )
                    )
                )) # main panel
    )
    )        # fluidpage





# ),
#                         tabPanel("Navbar 2", checkboxInput("chk1", "Checkbox 1"), bookmarkButton(id = "bookmark1"), verbatimTextOutput("summary.min")),
#                                        
#                         tabPanel("Navbar 3", checkboxInput("chk2", "Checkbox 2"), bookmarkButton(id = "bookmark2"), verbatimTextOutput("summary.end")),
#                                            # class="pt-page pt-page-3"),
#                         tabPanel("Navbar 4" ),
#                                           # class="pt-page pt-page-4"),
#                         tabPanel("Navbar 5"), 
#                                          # class="pt-page pt-page-5"),
#                        navbarMenu("Navbar 6",
#                             HTML('<li class="dropdown">
#           <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false">Dropdown <span class="caret"></span></a>
#                                  <ul class="dropdown-menu">
#                                  <li><a href="#">Action</a></li>
#                                  <li><a href="#">Another action</a></li>
#                                  <li><a href="#">Something else here</a></li>
#                                  <li role="separator" class="divider"></li>
#                                  <li><a href="#">Separated link</a></li>
#                                  </ul>
#                                  </li>')
#                              ),
#                        navbarMenu("Menu", HTML('<li><a href="http://google.com">Link</a></li>'),
#                                   HTML('<li><a href="#plotttt">Link</a></li>'),
#                                   HTML('<li><a href="#tab-jquery02">Link</a></li>'),
#                                        tabPanel("Panel 1.1", 
#                                               HTML('<li><a href="http://google.com">Link</a></li>')
#                                                                                                        ),
#                                   "----",
#                                        tabPanel("Panel 1.2")),
#              tabPanel("Navbar 7")
#   )
  
  )  ## End tagList

}


























#################################################################
#################################################################
#################################################################


# shinyServer(function(input, output, session){
server <- function(input, output, session) {
  
options(shiny.sanitize.errors = F)  # the original error message that you don’t want the user to see
options(shiny.maxRequestSize=100*1024^2)  # By default, Shiny limits file uploads to 5MB per file.  -->100M
  
   # observeEvent(input$reset_input, {
  #   shinyjs::reset("side-panel")
  # })




####### Tooltip test ###############




############ Input test ####################  
peek <- function(d, x = ncol(d))
{
  if(x >= 6) x=6
  if (data.table::is.data.table(d)) d[1:3, 1:x, with = FALSE]
  else if (is.data.frame(d) | is.matrix(d)) d[1:3, 1:x] 
} 



info <- eventReactive(input$choice, {
  inFile2 <- input$file
  # Instead # if (is.null(inFile)) ... use "req"
  req(inFile2)
  
  # Determine document format;    https://groups.google.com/forum/#!topic/shiny-discuss/Mj2KFfECBhU
  ptn <- "\\.[[:alnum:]]{1,5}$"
  suf <- tolower(regmatches(inFile2$name, regexpr(ptn, inFile2$name)))
   
    # Options for Excel documents;     
  if (suf %in% c('.xls', '.xlsx')) {
  
  ext <- tools::file_ext(inFile2$name)
  file.rename(inFile2$datapath,
              paste(inFile2$datapath, ext, sep="."))
  f <- read_excel(paste(inFile2$datapath, ext, sep="."), 1)
  vars <- names(f)
  # Update select input immediately after clicking on the action button. 
  updateSelectInput(session, "columns","Select Columns", choices = vars)
  f
    }  else {    
      # Changes in read.table 
  f <- read.table(inFile2$datapath, header = input$header, sep = input$sep, quote = input$quote)
  vars <- names(f)
  # Update select input immediately after clicking on the action button. 
  updateSelectInput(session, "columns","Select Columns", choices = vars)
  f
    }
})

output$table_display <- renderTable({
  f <- info()
  f <- subset(f, select = input$columns) #subsetting takes place here
  peek(f)
})
  




  
 
#################################################################################  
 ######################   heatmap & circos plot test   ######################### 
  
  
plot <- reactiveValues()
  
observeEvent(input$ok, {
  mat = cbind(rbind(matrix(rnorm(16, -1), 4), matrix(rnorm(32, 1), 8)),
              rbind(matrix(rnorm(24, 1), 4), matrix(rnorm(48, -1), 8)))
  
  rownames(mat) = paste0("R", 1:12)
  colnames(mat) = paste0("C", 1:10)
  
  plot$mat <- mat
  })
  
  
  output$plotcircos <- renderPlot({
    n = 1000
    a = data.frame(factor = sample(letters[1:8], n, replace = TRUE),
                   x = rnorm(n), y = runif(n))
    circos.par("track.height" = 0.1)
    circos.initialize(factors = a$factor, x = a$x)
    circos.trackPlotRegion(factors = a$factor, y = a$y,
                           panel.fun = function(x, y) {
                             circos.axis()
                           })
    col = rep(c("#FF0000", "#00FF00"), 4)
    circos.trackPoints(a$factor, a$x, a$y, col = col, pch = 16, cex = 0.5)
    circos.text(-1, 0.5, "left", sector.index = "a", track.index = 1)
    circos.text(1, 0.5, "right", sector.index = "a")
  })
  
  
  
  output$mat <- renderTable({
   Heatmap(plot$mat)
  })  
  
  
  
  
  
  
  
  
  # login   &  bookmark server test  #########################   
  # ---- For advanced user... ----  
  # ---- Session Manager saves and restores the state of all or some..... ----   
  
  
# USER <- reactiveValues(Logged = Logged)
log <- reactiveValues()
observeEvent(input$Login, {
   lll<-input$Login
   if (!is.null(lll)) { 
     if (input$Login > 0) {
         Password <- isolate(input$password)
         if(my_password == Password) {log$loglo = c(2,3,4,4,5 )
         removeModal()
         } else { log$loglo =NULL}
         }}
    })
  
 output$loglog <- renderPrint({
   log$loglo
 })
  
 # output$bookmark <- renderUI({
 #   lll<-input$Login
 #   if (!is.null(lll)) { 
 #     if (input$Login > 0) {
 #       Password <- isolate(input$password)
 #       if(my_password == Password) { bookmarkButton() 
 #        
 #       } else { return(NULL) }
 #     }}
 #    })
 

 
 #### Modal dialogue 
 
vals <- reactiveValues(data = NULL)
dataModal <- function(failed = FALSE) {
   modalDialog(
     textInput("dataset", "Admission Code",
               placeholder = 'Add code for sign-in'
     ),
     span('(Sign in for Session Manager : automatically saves and restores the state of server processing',
          ')'),
     if (failed)
       div(tags$b("Check the code for sign in", style = "color: orange;")),
     easyClose = TRUE,
     footer = tagList(
       modalButton("Cancel"),
       actionButton("ok", "OK")
     )
   )
 }
 
 # Show modal when button is clicked.

observeEvent(input$show, {
   showModal(dataModal())
 })
 
 # When OK button is pressed, attempt to load the data set. If successful,
 # remove the modal. If not show another modal, but this time with a failure
 # message.

# observeEvent(input$dataset, {
#     onevent("keydown", "dataset", function(event) {
#      if (event$keyCode == 13) {if (!is.null(input$dataset) && nzchar(input$dataset) &&
#                                    exists(input$dataset) && is.data.frame(get(input$dataset))) {
#        vals$data <- get(input$dataset)
#        removeModal()
#      } else {
#        showModal(dataModal(failed = TRUE))
#      }
#        }})
#   })
#  
#  
#  observeEvent(input$ok, {
#    # Check that data object exists and is data frame.
#    if (!is.null(input$dataset) && nzchar(input$dataset) &&
#        exists(input$dataset) && is.data.frame(get(input$dataset))) {
#      vals$data <- get(input$dataset)
#      removeModal()
#    } else {
#      showModal(dataModal(failed = TRUE))
#    }
#  })
 
observeEvent(input$dataset, {
   onevent("keydown", "dataset", function(event) {
     if (event$keyCode == 13) {if (!is.null(input$dataset) && nzchar(input$dataset) && input$dataset == "kkumedicine" ) {
       vals$data <- "kkumedicine"
       showElement("txtEditorr")
       hideElement("loglog")
       showElement("outout")
       removeModal()
       
     } else { showElement("loglog")
              showModal(dataModal(failed = TRUE))       # bug report : screen width decrease.....
     }
     }})
 })
 
 
observeEvent(input$ok, {
   # Check that data object exists and is data frame.
   if (!is.null(input$dataset) && nzchar(input$dataset) && input$dataset == "kkumedicine" ) {
     vals$data <- "kkumedicine"
     removeModal()
    } else {
     showModal(dataModal(failed = TRUE))
   }   })
 
 
output$uploadbutton <- renderUI({
   if (input$demo != "" | !is.null(input$txtInput)) return(NULL)
   actionButton("showupload", "upload", icon = shiny::icon("link", lib ="glyphicon"), title = "")
 })
 
 
output$bookmark <- renderUI({
   if (is.null(vals$data)) {return(NULL)} else {
   bookmarkButton(label = "Bookmark Server...", icon = shiny::icon("link", lib ="glyphicon"), title = "Bookmark this application's state and get a URL for sharing.") 
     
     }
    })
output$hline <- renderUI({
   if (is.null(vals$data)) {return(NULL)} else { hr()
   }     })


# Display information about selected data
output$dataInfo <- renderPrint({
   if (is.null(vals$data))
     "No data selected"
   else
     summary(vals$data)  })
 
 
 
 # RMySQL test  #########################
output$tbl <- DT::renderDataTable({
   conn <- dbConnect(RMySQL::MySQL(), user="root", password="tjqjqlalf",dbname="lamp", host="127.0.0.1")
   on.exit(dbDisconnect(conn), add = TRUE)
   dbGetQuery(conn, paste0( 
     "SELECT * FROM test ", paste0("LIMIT ", input$nrows)))
 })
 
# RMySQL basic syntax  : http://kwonnam.pe.kr/wiki/database/mysql/basic
# SELECT * FROM test WHERE test01 < 90;
# SELECT * FROM test ORDER BY test01 DESC;   # ASC 
# INSERT INTO / UPDATE / DELETE      tablename (col1, col2, ...) VALUES(값1, 값2, ...); 
 

###### Local file system (local) , Amazon S3 (remote) :https://shiny.rstudio.com/articles/persistent-data-storage.html
 outputDir <- "responses"
 
 saveData <- function(data) {
   data <- t(data)
   # Create a unique file name
   fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
   # Write the file to the local system
   write.csv(
     x = data,
     file = file.path(outputDir, fileName), 
     row.names = FALSE, quote = TRUE
   )
 }
 
 loadData <- function() {
   # Read all the files into a list
   files <- list.files(outputDir, full.names = TRUE)
   data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
   # Concatenate all data together into one data.frame
   data <- do.call(rbind, data)
   data
 }
 
##### MySQL (local or remote) 
 options(mysql = list(
   "host" = "127.0.0.1",
   # "port" = 3306,
   "user" = "root",
   "password" = "tjqjqlalf"
 ))
 databaseName <- "lamp"
 table <- "test"
 
saveData <- function(data) {
   # Connect to the database
   db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                   user = options()$mysql$user, password = options()$mysql$password) #  port = options()$mysql$port,
   # Construct the update query by looping over the data fields
   query <- sprintf(      # INSERT INTO  tablename (col1, col2, ...) VALUES(값1, 값2, ...); 
     "INSERT INTO %s (%s) VALUES ('%s')",
     table, 
     paste(names(data), collapse = ", "),
     paste(data, collapse = "', '")
   )
   # Submit the update query and disconnect
   dbGetQuery(db, query)
   dbDisconnect(db)
 }
 
 loadData <- function() {
   # Connect to the database
   db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                   user = options()$mysql$user, password = options()$mysql$password) #  port = options()$mysql$port,
   # Construct the fetching query
   query <- sprintf("SELECT * FROM %s", table)
   # Submit the fetch query and disconnect
   data <- dbGetQuery(db, query)
   dbDisconnect(db)
   data
 }
 


 
 
  ################################## phase I : file upload & model constructing
val <- reactiveValues()
  

  


  
  
  
  
  ######## toggle UI ######
# reactive는 실시간함수 & 변수

rrr=reactive({ if (input$demo == "" && is.null(input$txtInput)) {hideElement("abs"); hideElement("uploadcheckbox"); showElement("uploadtoggle"); return(NULL)} 
      hide("testtest")    # toggle/hide/show
      hide("testtestt", anim =T)
      hide("testtesttt")
      hide("testtestttt")
      # showElement("abs", anim =T)
      hideElement("uploadtoggle", anim =T)
      showElement("uploadcheckbox", anim =T)
   
      if (input$uploadcheck == F) { hideElement("uploadtoggle"); return(NULL)} else {
      showElement("uploadtoggle", anim =T)}   
  
      })    
  
# observe는 ()안 실시간 모두 실행, observeEvent는 event있을시
observe({ rrr() })



rrrr=reactive({
if (is.null(vals$data)) { hideElement("txtEditorr"); return(NULL)} else {
  showElement("txtEditorr")
  showElement("gprbuttonshow")}
})
observe({ rrrr() })


rrrrr=reactive({
  if (!is.null(values$gg))
    showElement("abs")
})

observe({ rrrrr() })



# observeEvent(input$txtInput,{
#   observe({ if (input$demo == "" && is.null(input$txtInput)) return(NULL)
#     toggle("testtest")})
#   })  
 
  
  
  # observe({
  #   if (input$demo != "" && !is.null(input$txtInput)) return(NULL)
  #   toggle("testtest")
  # })

 
  
  
  
  
observeEvent(input$demo, {
    if (input$demo == "") { return(NULL)
    # shinyjs::reset("side-panel")
    shinyjs::reset("reset")
    shinyjs::reset("reset1")
    if (!is.null(val$printreact))
      val$printreact = NULL
    if (!is.null(val$mod))
      val$mod = NULL
    if (!is.null(val$mo))
      val$mo = NULL } else {
  isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "modeling", value = 0)
      for (i in 1:10) {
        progress$inc(1 / 10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    path=paste0(input$demo, ".xml")
    val$mod <- readSBMLmod(paste0("./www/data/",path))
 }
 })


  output$bound <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    react <- mod@react_id
    checkboxGroupInput("reaction", "Objective function(s)", react)
  })

  cat("Got here", file = stderr())

output$bound01 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test2",
      "O2",
      min = -1000,
      max = 1000,
      value = c(mod@lowbnd[33], mod@uppbnd[33])
    )
  })

output$bound02 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test3",
      "O2",
      min = -1000,
      max = 1000,
      value = c(mod@lowbnd[33], mod@uppbnd[33])
    )
  })
  
  
output$bound03 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test4",
      "GLUNm",
      min = 0,
      max = 1000,
      value = c(mod@lowbnd[71], mod@uppbnd[71])
    )
  })

output$bound04 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test5",
      "HEX1",
      min = 0,
      max = 1000,
      value = c(mod@lowbnd[73], mod@uppbnd[73])
    )
  })
  
  
output$bound05 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test6",
      "PFK",
      min = 0,
      max = 1000,
      value = c(mod@lowbnd[19], mod@uppbnd[19])
    )
  })
  
  
output$bound06 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test7",
      "PYK",
      min = 0,
      max = 1000,
      value = c(mod@lowbnd[24], mod@uppbnd[24])
    )
  })
  
output$bound07 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test8",
      "LDH_L",
      min = -1000,
      max = 1000,
      value = c(mod@lowbnd[16], mod@uppbnd[16])
    )
  })
  
output$bound08 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    sliderInput(
      "test9",
      "PDHm",
      min = 0,
      max = 1000,
      value = c(mod@lowbnd[72], mod@uppbnd[72])
    )
  })
  


output$Flux <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    selectInput(
      "X_Flux",
      "X_Flux",
      c("Glucose_Flux" = "Glucose_Flux",
        "Glutamine_Flux" = "Glutamine_Flux"),
      selected = "Glucose_Flux"
    )
  })

output$go <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    actionButton("go", "Submit", onclick = "location.href='#top'", class = "btn-primary") # icon=icon("spotify"),
  })

output$hline1 <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) {return(NULL)} else { hr()
    }
  })
  
  
  
  
output$addreact <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    wellPanel(
      checkboxInput("addreact", "Add reaction"),
      conditionalPanel(
        condition = "input.addreact == true",
        textInput("id", "reaction ID"),
        textInput("met", "metabolite"),
        textInput("scoef", "S-coef"),
        checkboxInput("reversible", "reversible"),
        textInput("lb", "Lower bound",0),
        textInput("ub", "Upper bound", 1000),
        textInput("obj", "Objective coefficient", 0),
        hr(),
        actionButton("go1", "Add reaction", onclick = "location.href='#top'")
      
        #, onclick = "location.href='#reactend'")
        # selectInput("S", "Method", list("lm", "glm", "gam", "loess", "rlm"))
      )
    )
  })

output$addex <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    wellPanel(
      checkboxInput("addexx", "Add Exchange Reaction"),
      conditionalPanel(
        condition = "input.addexx == true",
        textInput("exmet", "metabolite"),
        textInput("exlb", "Lower bound",0),
        textInput("exub", "Upper bound", 1000),
        hr(),
        actionButton("addex", "Add Ex_reaction",  onclick = "location.href='#top'")
        # helpText('for jun jaejung')
        
        
        #, onclick = "location.href='#reactend'")
        # selectInput("S", "Method", list("lm", "glm", "gam", "loess", "rlm"))
      )) })
  
  

  
  
  
  
output$printreact <- renderUI({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo 
    removeUI(
      selector = "div:has(> #txt)"
    )}
    l = printReaction(mod, mod, 1:length(mod@react_id))
    l = unlist(strsplit(l, "\t"))
    try(val$printreact <-
          data.frame(reac_id = l[seq(1, length(l), 2)], equation = l[seq(2, length(l), 2)]))
    actionButton("go2", "Print reaction", onclick = "location.href='#top'")
  })
  
 
  S <- reactive({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo  }
    a=as.matrix(mod@S[1:30, 1:30])
    x=xtable(a,align=rep("",ncol(a)+1))
    p(style= "font-size: 9px", paste0(paste0("$$", print(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)),"$$"))
  
  })
  
  # output$Smatrixx<- renderUI({
  #   if (input$demo == "" && is.null(input$txtInput)) return(NULL)
  #   mod <- val$mod
  #   if (!is.null(val$mo)) { mod <- val$mo  }
  #   a=as.matrix(mod@S)     #  a=as.matrix(mod@S[1:20, 1:20])
  #   x=xtable(a,align=rep("",ncol(a)+1), digits=0)
  #  
  #   withMathJax(p(style= "font-size: 9px", paste0(paste0("$$", print(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)),"$$")))
  #   
  # })
  
 
output$Smatrix<- DT::renderDataTable({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo  }
    smat=as.matrix(mod@S)    
    rownames(smat) <- mod@met_name
    colnames(smat) <- mod@react_id
    smat
  }, extensions = 'FixedColumns', options = list(lengthMenu = c(10, 30, 100), pageLength = 30,  searchHighlight = T, scrollX = T, fixedColumns = T, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"))
  )
  
output$reactname<- DT::renderDataTable({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo  }
    reactname <-data.frame( React_id = mod@react_id,  React_name = mod@react_name ,React_lowbnd=mod@lowbnd, React_uppbnd=mod@uppbnd )
    sub<-as.matrix(mod@subSys)
    subsys=NULL
   for( i in 1: length(colnames(sub))) {inx=NULL; inx=which(sub[,i]==T); tem<-reactname[inx,]; tem$SubSystem <-colnames(sub)[i]; subsys=rbind(subsys, tem)}
    subsys      # colnames(sub
   },
    options = list(lengthMenu = c(10, 30, 100), pageLength = 30,  searchHighlight = TRUE,  initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"))
  )
  
  
output$gpr<- DT::renderDataTable({
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo  }
    gpr=as.data.frame(mod@gpr)   
    rownames(gpr) <- mod@react_id
    colnames(gpr) <- c("GeneProteinReaction")
    gpr
  }, extensions = 'FixedColumns', options = list(lengthMenu = c(10, 30, 100), pageLength = 30,  searchHighlight = T, scrollX = T, fixedColumns = T, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"))
  )
  
  
  
  
  
   
 ################# demo end ###################### 
  
  
  
observeEvent(input$txtInput, {
    inFile <- input$txtInput
    if (is.null(inFile))
      return(NULL)
    # shinyjs::reset("side-panel")
    shinyjs::reset("reset")
    shinyjs::reset("reset1")
    if (!is.null(val$printreact))
      val$printreact = NULL
    if (!is.null(val$mod))
      val$mod = NULL
    if (!is.null(val$mo))
      val$mo = NULL
    
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "modeling", value = 0)
      for (i in 1:10) {
        progress$inc(1 / 10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    
    val$mod <- readSBMLmod(inFile$datapath)
    
    })
  
  # output$bound <- renderUI({
  #   if (is.null(input$txtInput))
  #     return(NULL)
  #   mod <- val$mod
  #   react <- mod@react_id
  #   checkboxGroupInput("reaction", "Objective function(s)", react)
  # })

  # cat("Got here", file = stderr())
  # 
  # output$bound01 <- renderUI({
  #   inFile <- input$txtInput
  #   if (is.null(inFile))
  #     return(NULL)
  #   mod <- val$mod
  #   sliderInput(
  #     "test2",
  #     "O2",
  #     min = -1000,
  #     max = 1000,
  #     value = c(mod@lowbnd[33], mod@uppbnd[33])
  #   )
  # })
  # 
  # output$bound02 <- renderUI({
  #   inFile <- input$txtInput
  #   if (is.null(inFile))
  #     return(NULL)
  #   mod <- val$mod
  #   sliderInput(
  #     "test3",
  #     "O2",
  #     min = -1000,
  #     max = 1000,
  #     value = c(mod@lowbnd[33], mod@uppbnd[33])
  #   )
  # })
  # 
  # 
  # output$Flux <- renderUI({
  #   if (is.null(input$txtInput))
  #     return(NULL)
  #   selectInput(
  #     "X_Flux",
  #     "X_Flux",
  #     c("Glucose_Flux" = "Glucose_Flux",
  #       "Glutamine_Flux" = "Glutamine_Flux"),
  #     selected = "Glucose_Flux"
  #   )
  # })
  # 
  # output$go <- renderUI({
  #   if (is.null(input$txtInput))
  #     return(NULL)
  #   actionButton("go", "Submit", onclick = "location.href='#top'", class = "btn-primary") # icon=icon("spotify"),
  # })
  # 
  # output$addreact <- renderUI({
  #   inFile <- input$txtInput
  #   if (is.null(inFile))
  #     return(NULL)
  #   mod <- val$mod
  #   wellPanel(
  #     checkboxInput("addreact", "Add reaction"),
  #     conditionalPanel(
  #       condition = "input.addreact == true",
  #       textInput("id", "reaction ID"),
  #       textInput("met", "metabolite"),
  #       textInput("scoef", "S-coef"),
  #       checkboxInput("reversible", "reversible"),
  #       textInput("lb", "Lower bound", 0),
  #       textInput("ub", "Upper bound", 1000),
  #       textInput("obj", "Objective coefficient", 0),
  #       br(),
  #       actionButton("go1", "Add reaction", onclick = "location.href='#reactend'")
  #       # selectInput("S", "Method", list("lm", "glm", "gam", "loess", "rlm"))
  #     )
  #   )
  # })
  # 
  # output$printreact <- renderUI({
  #   if (is.null(input$txtInput))
  #     return(NULL)
  #   actionButton("go2", "Print reaction", onclick = "location.href='#reactstart'")
  # })
  # onclick ="window.open('http://google.com', '_blank')"
  
  
  
  ################################# phase II  observeEvent & output

  

aaa <- function(x) eval(as.symbol("1 + 1"))





shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      runcodeUI(code = "shinyjs::alert('Hello!')", type = "ace"),
     textOutput("test")
    ),
    server = function(input, output) {
      
      vvv <- reactiveValues()
   runcodeServer()
    
   output$test<- renderText({ 
    eval(input$runcode_expr)})
    }
  )


# runCode 할 수 있지만 위험 서버를 망가뜨릴 수 있기 때문에 !!!!!!      ?runcodeServer
# of course that runcodeServer is an eval, and therefore a huge security risk.





  
observeEvent(input$go1, {
     mod <- val$mod
    if (!is.null(val$mo))
      mod <- val$mo
     #val$addreactid = input$id
    val$mo <- sybil::addReact(
            model = mod,
            id = input$id,
            reversible=input$reversible,
            lb=as.numeric(unlist(strsplit(input$lb, ","))),
            ub=as.numeric(unlist(strsplit(input$ub, ","))),
            obj=as.numeric(unlist(strsplit(input$obj, ","))),
            met = unlist(strsplit(input$met, ",")),
            Scoef = as.vector(as.numeric(unlist(strsplit(
              input$scoef, ","
            )))
          ))
    
    
    #, reversible=input$reversible, lb=input$lb, ub=input$ub, obj=input$obj)   # as.numeric(unlist(strsplit(input$vec1,",")))
    
  })
  
  
observeEvent(input$addex, {
    mod <- val$mod
    if (!is.null(val$mo))
      mod <- val$mo

    val$mo <- sybil::addExchReact(
      model = mod,
      lb=as.numeric(unlist(strsplit(input$exlb, ","))),
      ub=as.numeric(unlist(strsplit(input$exub, ","))),
      met = unlist(strsplit(input$exmet, ","))
    )
    })
  
  
 
  
  
  
observeEvent(input$go1, {
    mod <- val$mod
    if (!is.null(val$mo)) { mod <- val$mo 
    removeUI(
      selector = "div:has(> #txt)"
    )}
    l = printReaction(mod, mod, 1:length(mod@react_id))
    l = unlist(strsplit(l, "\t"))

    try(val$printreact <-
          data.frame(reac_id = l[seq(1, length(l), 2)], equation = l[seq(2, length(l), 2)]))
    showreactfunc()
  })
  


  
observeEvent(input$go2, {
    mod <- val$mod
    if (!is.null(val$mo))
      mod <- val$mo
    l = printReaction(mod, mod, 1:length(mod@react_id))
    l = unlist(strsplit(l, "\t"))
    try(val$printreact <-
          data.frame(reac_id = l[seq(1, length(l), 2)], equation = l[seq(2, length(l), 2)]))
    showreactfunc.0()  
  })
  

 
  observeEvent(input$txtInput, {
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    updateTabsetPanel(session, "inTabset", selected = "Meta-info")
  })      # updateNavbarPage  
  
  
   observeEvent(input$demo, {
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    updateTabsetPanel(session, "inTabset", selected = "Meta-info")
  })      # updateNavbarPage
  
  
  observeEvent(input$go2, {
    updateTabsetPanel(session, "inTabset", selected = "Reactions")
  })      # updateNavbarPage
  
 
  observeEvent(input$go, {
    updateTabsetPanel(session, "inTabset", selected = "Results")
  })      # updateNavbarPage
  
  observeEvent(input$go1, {
    updateTabsetPanel(session, "inTabset", selected = "Reactions")
  })      # updateNavbarPage updateSliderInput(session,   updateTextInput(session, ..... 
   
  observeEvent(input$addex, {
    updateTabsetPanel(session, "inTabset", selected = "Reactions")
  })      # updateNavbarPage updateSliderInput(session,   updateTextInput(session, ..... 
  
  
  values <- reactiveValues()
  
  
  observeEvent({ 
    input$goabs
    input$go 
    }, {
    if (input$demo == "" && is.null(input$txtInput)) return(NULL)
    
     isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "modeling", value = 0)
      for (i in 1:10) {
        progress$inc(1 / 10, detail = "...")
        Sys.sleep(0.2)
      }
      })
    
   
    model <- val$mod
    if (!is.null(val$mo)) model <- val$mo
    
   
   
    model <- changeObjFunc(model, "ATP_consumption")
    if (!is.null(input$reaction))
      model <- changeObjFunc(model, input$reaction)
    
    
    values$test <- model@react_id
    values$lowbnd <- model@lowbnd
    values$uppbnd <- model@uppbnd
 
   #  model<- addExchReact(model, c("METX[e]"), lb=0, ub=0.4)
   # #   ###  model <- addReact(model, id="VOLUME_CONSTRAINT", met=c("METX[e]"),  Scoef=c(-1), reversible=F, lb=0, ub=0.4, obj=0)
   # #   # 
   #  
   #  
   #   model <- addReact(model, id="LDH_L", met=c("lac_DASH_L[e]", "nad[e]", "h[e]","nadh[e]","pyr[e]","METX[e]"),  Scoef=c(-1,-1,1,1,1,-0.00046), reversible=T, lb=-1000, ub=1000, obj=0)
   #   model <- addReact(model, id="GLUNm", met=c("h2o[e]","gln_DASH_L[e]","glu_DASH_L[e]","nh4[e]","METX[e]"),  Scoef=c(-1,-1,1,1,0.2), reversible=F, lb=0, ub=1000, obj=0)
   #   model <- addReact(model, id="PDHm", met=c("coa[e]","nad[e]","pyr[e]","accoa[e]","co2[e]","nadh[e]","METX[e]"),  Scoef=c(-1,-1,-1,1,1,1,0.2), reversible=F, lb=0, ub=1000, obj=0)
   #   model <- addReact(model, id="HEX1", met=c("atp[e]","glc_DASH_D[e]","adp[e]","g6p[e]","h[e]","METX[e]"),  Scoef=c(-1,-1,1,1,1,0.0027), reversible=F, lb=0, ub=1000, obj=0)

    
    
    # if(Sys.info()['sysname'] != "Windows"){
    #   require("doMC")
    #   registerDoMC(detectCores() - 1)
    # }else{
    #   require("doParallel")
    #   cl <- makeCluster(detectCores() - 1)
    #   registerDoParallel(cl)
    #   #snow is also an option
    # }
   
    result = c()
    bb = NULL
    start = 0
    end = 6
    by = 0.2
    num = ((end - start) / by) + 1
   # input이 있는 반응형에서는 serial하게 해야됨, %do%로 하고 input형 아닌경우       %dopar% 로 하면 됨.  .combine=c
    #  foreach(i=seq(start, end, by),.combine=c, .export=c('changeBounds', 'optimizeProb'), .packages='sybil') %do% {
    for (i in seq(start, end, by)) {
      model <-
        changeBounds(
          model,
          c(input$X_Flux, "O2_Flux"),
          lb = c(0, input$test2[1]),
          ub = c(i, input$test2[2])
        ) # "Glucose_Flux"  "PDHm", "PYK", "ICDHxm", "LDH_L",  0, 0, 0, -1000,1000, 1000, 1000, 1000,
      
      if (!is.null(input$test3) | !is.null(input$test4) |!is.null(input$test5) |!is.null(input$test6) |!is.null(input$test7) |!is.null(input$test8) |!is.null(input$test8)  ) 
        model <-
          changeBounds(
            model,
            c("O2_Flux", "GLUNm", "HEX1", "PFK", "PYK", "LDH_L","PDHm"),
            lb = c(input$test3[1], input$test4[1],input$test5[1], input$test6[1], input$test7[1], input$test8[1], input$test9[1]),
            ub = c(input$test3[2], input$test4[2], input$test5[2], input$test6[2], input$test7[2], input$test8[2], input$test9[2])
          )
      
      optL <- optimizeProb(model, algorithm = "fba", retOptSol = F)
      aa = data.frame(i, optL$obj, model@react_id, optL$fluxes)
      bb = list(i, aa)
      result = c(result, bb)
      print(bb)
    }
    # stopCluster()
    
    
    tar = c(
     # "GND",
     # "CSm",
     # "Palmitic_Acid_consumption",
     # "SERINE_PRODUCTION",
     # "TYROSINE_PRODUCTION",
      "HEX1",
      "PFK",
      "PYK",
    # "GLUNm",
     # "GLYCINE_PRODUCTION",
    #  "ICDHxm",
    #  "G6PDH2r"
    "PDHm",
    "LDH_L",
    "Lactate_outflux"
   # "ATP_consumption"
    
    ) #, "VOLUME_CONSTRAINT", "FAS8", "ACC")#, biomass")
    
    num2 = seq(2, 2 * num, 2)
    dd = NULL
    ee = NULL
    for (j in 1:length(tar)) {
      for (i in num2) {
        cc = result[[i]][which(result[[i]][, 3] %in% tar[j]), c(1, 2, 4)]
        dd = rbind(dd, cc)
      }
      if (j == 1) {
        ee = dd
      } else {
        ee = cbind(ee, dd[, 3])
      }
      dd = NULL
    }
    colnames(ee) <- c("G", "Obj", tar)
    df = melt(ee, id = "G")
    rownames(ee) <- format(round(ee[,1], 2), nsmall = 2)
    ee <- ee[,-1]
    ee <- round(ee, digits=2)
    # ee<-format(round(ee, 2), nsmall = 2)
    df$variable; df_sub <- df[c(31, 62, 93, 124, 155, 186, 217, 248), ] 
    values$ee <- ee
    values$summary.end = summary(model)
    gg <- ggplot(df) + geom_line(aes(x = G, y = value, color = variable),size=0.8) + theme_bw()
    gg + geom_label(aes(x=G, y=value, label=variable), size=5, data=df_sub)  
    values$gg <- gg + geom_label_repel(aes(x=G, y=value, label=variable), size=4, fontface = 'bold', data=df_sub)
    showElement("fbashow")
 
   })
 
  
  
   
  
  output$testtesttest <- renderTable({
    data.frame(v=values$test, l=values$lowbnd, u=values$uppbnd)
    
  }) 
  
  output$plotttt <- renderPlot({
    # ggsave("plot.pdf", values$gg)
    values$gg
  }) 

  # plot UI   ---> http://stackoverflow.com/questions/41120696/shiny-plotoutput-dynamic-properties
  
  
  output$tttt <- DT::renderDataTable({
  
    dat <- datatable(  
      values$ee
  , extensions = c('FixedColumns','Buttons'),
  options = list(scrollX='800px', dom = 'Bfrtip', buttons = 
                   list('copy', 'print', list(
                     extend = 'collection',
                     buttons = c('csv', 'excel', 'pdf'),
                     text = 'Download'
                   )),lengthMenu = c(10, 50, 100), pageLength = 50,  searchHighlight = T, scrollX = T, fixedColumns = list(leftColumns = 2), initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"))
    )%>%
      formatStyle('Obj',  background = styleColorBar(values$ee$Obj, 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    return(dat)
  
  })  
  # https://rstudio.github.io/DT/functions.html
  # https://rstudio.github.io/DT/007-search.html
  # filter = 'bottom'
  # %>% formatStyle('Obj',  color = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  
  # fixedColumns = list(leftColumns = 2, rightColumns = 1)      # dom = 't',
  # columnDefs = list(list(targets = c(1, 3), searchable = FALSE)),
  
  
  
  
 
  output$showreaction <- DT::renderDataTable(
    { val$printreact }, options = list(lengthMenu = c(10, 50, 100), pageLength = 50,  searchHighlight = TRUE,   initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"))    # order = list(1, 'desc'), search = list( search = val$addreactid ),
  )
  
 
  showreactfunc.0 <-reactive({   
    output$showreaction <- DT::renderDataTable(    
      { val$printreact }, options = list(lengthMenu = c(10, 50, 100), pageLength = 50,  searchHighlight = TRUE,  initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"))
    )          
  })  
  
  
   
  
  showreactfunc <-eventReactive(input$go1,{   
  output$showreaction <- DT::renderDataTable(    
  { val$printreact }, options = list(lengthMenu = c(10, 50, 100), pageLength = 50,  searchHighlight = TRUE,  search = list( search = input$id ), initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"))
  )          
  })
 # val$addreactid 


  
  
  #------------------ exp2flux---------------------------#
  gprval <- reactiveValues()
  
 
  
  
  observeEvent(input$gpr, {
     isolate({ progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "modeling", value = 0)
      for (i in 1:10) {
        progress$inc(1 / 10, detail = "...")
        Sys.sleep(0.2)} })
  
  # load("./www/data/luad.RData")
  # luad <-FireData 
  # expr <- luad@RNASeq2GeneNorm
    
  # expr <-  gprval$expr
  load("./www/data/luadExpr.RData")
  
   a=NULL
  a$aliastosymbol <- mapIds(org.Hs.eg.db,
                            keys=rownames(expr),
                            keytype="ALIAS",
                            column="SYMBOL",
                            multiVals="first")
  a$entrezwithna <- mapIds(org.Hs.eg.db,
                           keys=a$aliastosymbol,
                           keytype="SYMBOL",
                           column="ENTREZID",
                           multiVals="first")

  a$entrezwithna[sapply(a$entrezwithna, is.null)] <- NA
  rownames(expr) <- unlist(a$entrezwithna)
  expr=expr[which(rownames(expr) != "NA"), ]
  expr=expr[!duplicated(rownames(expr)), ]

  gprval$gprexpr<-expr
  
  tcgaSampleType <- function(x){
    code<-substring(x,14,15)
    if (code < 10){
      out <- "Tumor"
    } else if ((code == 10) || (code == 11) || (code == 13) || (code == 14)){
      out <- "Normal"
    } else if (code==20){
      out <- "Control"
    } else if (code==40){
      out <- "TRB"
    } else if (code==50){
      out <- "cell_line"
    } else if (code==60){
      out <- "XP"
    } else if (code==61){
      out <- "XCL"
    } else{
      out <- "other"
    }
    return(out)
  }

  type <- sapply(colnames(expr),tcgaSampleType)
  exprN<-expr[,which(type == "Normal")]
  exprT<-expr[,which(type == "Tumor")]

#----SBML
  
  warburg_crowd <- readSBMLmod("./www/data/warburg_crowd.xml")

  model=warburg_crowd
  model@react_id[74]; model@obj_coef[74]<-1

  model@gpr <- gsub("[()]", "", model@gpr)
  model@gpr <- gsub("\\.[0-9]+","", model@gpr)
  model@allGenes <- unique(gsub("\\.[0-9]+","", model@allGenes))

  expressionData <- ExpressionSet(assayData = exprN)
  expressionData2 <- ExpressionSet(assayData = exprT)
  
  
  my.exp2flux = 
    function (model, expression, organism = NULL, typeID = NULL, 
              missing = "mean", scale = FALSE, beta = 1000) 
    {
      
      # organism = NULL; typeID = NULL;  missing = "mean"; scale = FALSE 
      
      
      
      
      if (!is.null(organism) && !is.null(typeID)) {
        data <- try(kegg.gsets(species = organism, id.type = typeID))
        data <- matrix(gsub("[[:digit:]]+$", "", names(unlist(data$kg.sets))), 
                       dimnames = list(as.vector(unlist(data$kg.sets)), 
                                       c()))
      }
      gpr.expression <- function(gpr, expression, missing) {
        gpr <- gsub("[()]", "", gpr)
        gpr <- gsub("[[:space:]]", "", gpr)
        complex <- lapply(gpr, function(gpr) {
          unlist(strsplit(gpr, "or"))
        })
        genes <- lapply(complex, function(complex) {
          strsplit(complex, "and")
        })
        genes[lengths(genes) == 0] <- NA
        min.complex <- lapply(genes, function(gene) {
          lapply(gene, function(gene) {
            gene <- unlist(gene)
            if (!is.null(organism) && !is.null(typeID)) {
              if (!all(gene %in% rownames(data))) {
                gene <- gene[gene %in% rownames(data)]
              }
            }
            else {
              gene <- gene[gene %in% rownames(expression@assayData$exprs)]
            }
            if (length(gene) == 0) {
              minComplex <- 0
            }
            else {
              if (any(gene %in% rownames(expression@assayData$exprs))) {
                minComplex <- min(rowMeans(expression@assayData$exprs, 
                                           na.rm = TRUE)[gene], na.rm = TRUE)
              }
              else {
                if (!is.null(organism) && !is.null(typeID)) {
                  minComplex <- summary(rowMeans(expression@assayData$exprs, 
                                                 na.rm = TRUE)[names(data[data[, 1] %in% 
                                                                            names(sort(table(data[gene, ]))[1]), 
                                                                          ])])[[match(missing, c("min", "1q", "median", 
                                                                                                 "mean", "3q", "max"))]]
                }
                else {
                  minComplex <- 0
                }
              }
            }
            return(minComplex)
          })
        })
        exp <- unlist(lapply(min.complex, function(min.complex) {
          sum(unlist(min.complex), na.rm = TRUE)
        }))
        exp[exp == 0] <- summary(rowMeans(expression@assayData$exprs, 
                                          na.rm = TRUE))[[match(missing, c("min", "1q", "median", 
                                                                           "mean", "3q", "max"))]]
        return(exp)
      }
      exp <- gpr.expression(gpr = model@gpr, expression = expression, 
                            missing = missing)
      
      if (scale == TRUE) {
        exp <- round((exp/max(exp, na.rm = TRUE)), 6) * beta
      }
      
      #  model <- warburg_crowd 
      
      lb <- model@lowbnd
      ub <- model@uppbnd
      
      
      #  exp=1:75
      model@lowbnd[which(lb <= 0 & ub <= 0)] <- -1 * exp[which(lb <= 0 & ub <= 0)]
      model@uppbnd[which(lb >= 0 & ub >= 0)]<- 1 * exp[which(lb >= 0 & ub >= 0)]
      model@lowbnd[which(model@react_rev & lb <0)] <- -1 * exp[which(model@react_rev & lb <0)]
      model@uppbnd[which(model@react_rev & ub >0) ] <- 1 * exp[which(model@react_rev & ub >0) ]
      model@lowbnd[model@react_id %in% findExchReact(model)@react_id] <- lb[model@react_id %in% 
                                                                              findExchReact(model)@react_id]
      model@uppbnd[model@react_id %in% findExchReact(model)@react_id] <- ub[model@react_id %in% 
                                                                              findExchReact(model)@react_id]
      
     # model@lowbnd <- -1 * exp
      # model@lowbnd[!model@react_rev] <- 0
      # model@uppbnd <- exp
      # model@lowbnd[model@react_id %in% findExchReact(model)@react_id] <- lb[model@react_id %in% 
      #                                                                         findExchReact(model)@react_id]
      # model@uppbnd[model@react_id %in% findExchReact(model)@react_id] <- ub[model@react_id %in% 
      #                                                                         findExchReact(model)@react_id]
     return(model)
    }
  
  my.fluxDifferences =
    function (model1, model2, foldReport = 2, my.min.flux=0.01) 
    {
      f_m1 <- getFluxDist(optimizeProb(model1))
      f_m2 <- getFluxDist(optimizeProb(model2))
      if (identical(length(f_m1), length(f_m2))) {
        fold <- ((f_m2 - f_m1)/f_m1)
        fold[f_m1 == 0] <- f_m2[f_m1 == 0]
        fold[is.na(fold)] <- 0
        fold[is.infinite(fold)] <- 0
        min.flux = ((abs(f_m1) >= my.min.flux) & (abs(f_m2) >= my.min.flux))
        # different <- (abs(fold) >= foldReport)
        differentFlux <- matrix(cbind(f_m1, f_m2, fold), nrow = model1@react_num, 
                                dimnames = list(model1@react_id, c("fluxModel1", 
                                                                   "fluxModel2", "foldChange")))
        differentFlux <- differentFlux[min.flux, ]
        differentFlux <- differentFlux[abs(differentFlux[,3])>=foldReport,]
        
        return(differentFlux)
      }
      else {
        warning("Metabolic models must be the same with different restrictions")
      }
    }
  
  

  
  
  

  modelGE <- my.exp2flux(model = model,
                      expression = expressionData,
                      missing = "mean")
  modelGE2 <- my.exp2flux(model = model,
                       expression = expressionData2,
                       missing = "mean")
  gprval$modelGE <- modelGE
  gprval$modelGE2 <- modelGE2

  gprval$fluxDiff <- my.fluxDifferences(model1 = modelGE,
                   model2 = modelGE2,
                   foldReport = 0.7,
                   my.min.flux = 0.01)

  showElement("gprshow")
    
      
      })
  
  output$fluxDiff <- renderTable({
    ta<-gprval$fluxDiff
    as.data.frame(ta)
  }, include.rownames=T, digits = 5)
  

  output$fluxDifff <- renderTable({
    ta<-gprval$fluxDiff
    as.data.frame(ta)
  }, include.rownames=T)
  

  output$gprexpr <- DT::renderDataTable({
    dat <- datatable(  
      as.data.frame(gprval$gprexpr) 
      , extensions = c('FixedColumns','Buttons'),
      options = list(scrollX='800px', lengthMenu = c(10, 50, 100), pageLength = 20,  searchHighlight = T, scrollX = T, fixedColumns = list(leftColumns = 1), initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                         "}"))
    ) 
    return(dat)
  })  
 
  output$fluxDiffff <- DT::renderDataTable({
    dat <- datatable(  
      as.data.frame(gprval$fluxDiff)
      , extensions = c('FixedColumns','Buttons'),
      options = list(scrollX='800px', dom = 'Bfrtip', buttons = 
                       list('copy', 'print', list(
                         extend = 'collection',
                         buttons = c('csv', 'excel', 'pdf'),
                         text = 'Download'
                       )),lengthMenu = c(10, 50, 100), pageLength = 50,  searchHighlight = T, scrollX = T, fixedColumns = list(leftColumns = 1), initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                         "}"))
    ) 
    # %>% formatStyle('foldChange',  background = styleColorBar(values$ee$Obj, 'lightblue'),
    #               backgroundSize = '98% 88%',
    #               backgroundRepeat = 'no-repeat',
    #               backgroundPosition = 'center')
    return(dat)
  }) 
  
  
  
  
  
  output$igraph <- renderPlot({
    #-------igraph
    model1 = gprval$modelGE; model2 = gprval$modelGE2
    S <- as.matrix(t(model1@S))
    model1@met_id <- gsub("_DASH_", "",  model1@met_id)
    colnames(S) <- model1@met_id
    rownames(S) <- model1@react_id
    S[S < 0] <- -1
    S[S > 0] <- 1
    S[model1@react_rev, ][S[model1@react_rev, ] != 0] <- 2
    fD <- as.data.frame.array(fluxDifferences(model1 = model1,
                                              model2 = model2, foldReport = 0)[getFluxDist(optimizeProb(model1)) !=
                                                                                 0 & getFluxDist(optimizeProb(model2)) != 0, ])
    S <- S[getFluxDist(optimizeProb(model1)) != 0 & getFluxDist(optimizeProb(model2)) != 0, ]
    S <- S[, colSums(S != 0) != 0]
    types <- structure(c(rep(1, length(rownames(S))), rep(0, length(colnames(S)))), names = c(rownames(S), colnames(S)))
    edges <- NULL
    for (i in names(types)[types == 1]) {
      n_mets <- names(S[i, ])
      if (length(grep(2, S[i, ])) > 0) {
        edges <- c(edges, unlist(sapply(n_mets[S[i, ] !=
                                                 0], function(met) {
                                                   c(met, i)
                                                 })))
        edges <- c(edges, unlist(sapply(n_mets[S[i, ] !=
                                                 0], function(met) {
                                                   c(i, met)
                                                 })))
      }
      else {
        edges <- c(edges, unlist(sapply(n_mets[S[i, ] < 0],
                                        function(met) {
                                          c(met, i)
                                        })))
        edges <- c(edges, unlist(sapply(n_mets[S[i, ] > 0],
                                        function(met) {
                                          c(i, met)
                                        })))
      }
    }
    edges <- as.vector(unlist(as.vector(sapply(edges, function(edge) {
      which(names(types) == edge)
    }))))
    g <- igraph::make_bipartite_graph(types = types, edges = edges, directed = TRUE)
    V(g)$name <- names(types)
    V(g)$color <- ifelse(types == 1, "gray80", "gray90")
    V(g)$color[names(types) %in% rownames(fD)[fD$foldChange <
                                                0]] <- "red"
    V(g)$color[names(types) %in% rownames(fD)[fD$foldChange >
                                                0]] <- "green"
    V(g)$label.cex <- 0.7
    V(g)[type==0]$label.cex <- 0.5
    V(g)$shape="circle"
    V(g)[type==1]$shape <- "rectangle"  # "sphere"
    V(g)$label.color="grey"
    V(g)[type==1]$label.color <- "black"  # "sphere"
    vSizes <- types
    vSizes[types == 1] <- (abs(fD$foldChange)/max(abs(fD$foldChange))) *
      13
    vSizes[types == 0] <- 0.2
    V(g)$size <- vSizes
    # E(g)$width <- vSizes  # 이거 임시로 한것임   엣지에 weight줘야...
    layout <- layout.davidson.harel(g)
    igraph::plot.igraph(g, vertex.size = vSizes, vertex.label = names(types), vertex.label.family = "Helvetica", vertex.label.font= 2, vertex.label.color=V(g)$label.color, vertex.shape = V(g)$shape,  vertex.label.dist = V(g)$size*.025, vertex.label.cex = V(g)$label.cex,  edge.width=0.5, edge.arrow.size = 0.2) #vertex.shape="none"
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("my-report", '.csv', sep = '')
    },
    content = function(file) {
      write.csv(values$ee, file)
    }
  )
  
  
  # output$reportpdf = downloadHandler(
  #   filename = function() {
  #     "my-report.pdf"
  #   },
  #
  #   content = function(file) {
  #     # https://shiny.rstudio.com/articles/rmarkdown.html
  #
  #     src <- normalizePath("report.Rnw")
  #
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, "report.Rnw", overwrite = TRUE)
  #     out = knit2pdf("report.Rnw", clean = TRUE)
  #     file.rename(out, file) # move pdf to file for downloading
  #   } )
  
  
  
  output$downloadReport <- downloadHandler(
    # filename = function() { paste("my-report", '.pdf', sep='') },
    # content = function(file) {
    #   file.copy("plot.pdf", file, overwrite=TRUE)
    # }
    
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      # https://shiny.rstudio.com/articles/rmarkdown.html
      
      src <- normalizePath("report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format,
        PDF = pdf_document(),
        HTML = html_document(),
        Word = word_document()
      ))
      
      file.rename(out, file)
      # if (names(dev.cur()) != "null device") dev.off()
      # pdf(NULL)
    }
  )
  


output$downloadReport02 <- downloadHandler(
  # filename = function() { paste("my-report", '.pdf', sep='') },
  # content = function(file) {
  #   file.copy("plot.pdf", file, overwrite=TRUE)
  # }
  
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format02,
      PDF = 'pdf',
      HTML = 'html',
      Word = 'docx'
    ))
  },
  
  content = function(file) {
    # https://shiny.rstudio.com/articles/rmarkdown.html
    
    src <- normalizePath("report.Rmd")
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, "report.Rmd", overwrite = TRUE)
    
    library(rmarkdown)
    out <- render("report.Rmd", switch(
      input$format,
      PDF = pdf_document(),
      HTML = html_document(),
      Word = word_document()
    ))
    
    file.rename(out, file)
    # if (names(dev.cur()) != "null device") dev.off()
    # pdf(NULL)
  }
)
}


shinyApp(ui, server, enableBookmarking ="server")
