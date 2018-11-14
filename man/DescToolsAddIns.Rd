\name{RStudio AddIns}
\alias{Str}
\alias{Str}
\alias{Summary}
\alias{Abstract}
\alias{Head}
\alias{Example}
\alias{Some}
\alias{Desc}
\alias{Select}
\alias{Plot}
\alias{PlotD}
\alias{Save}
\alias{XLView}
\alias{IntView}
\alias{FileOpen}
\alias{FlipBackSlash}
\alias{SetArrow}
\alias{Enquote}
\alias{EnquoteS}
\alias{EvalEnquote}
\alias{BuildModel}
\alias{Edit}
\alias{NewObject}
\alias{InspectPnt}
\alias{Unclass}
\alias{GetExcelRange}
\alias{Class}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Some Functions to be Used as Shortcuts in 'RStudio'
%%  ~~function to do ... ~~
}
\description{
A simply great new feature in RStudio is the option to define add-ins and bind shortcuts to them.
This package includes add-ins for some of the most frequently used functions
in a data scientist's (at least mine) daily work (like \code{str()}, \code{example()}, \code{plot()}, \code{head()}, \code{view()}, \code{Desc()}).\cr
Most of these functions use the current selection in RStudios source panel and send the specific command to the console
where it is executed immediately.
Assigning shortcuts to these add-ins saves you quite a few keystrokes and brings a breeze of GUI-interaction into your life.
}
\usage{
Str()
Abstract()
Head()
Some()
Desc()
Summary()
Example()
Select()
BuildModel()
Plot()
PlotD()
Save()
XLView()
IntView()
FileOpen()
FlipBackSlash()
SetArrow()
Enquote()
EnquoteS()
EvalEnquote()
Edit()
NewObject()
InspectPnt()
Unclass()
GetExcelRange(env = .GlobalEnv)
Class()
}
\arguments{
\item{env}{Environment for GetExcelRange to write the data. GlobalEnv is default.}
}
\details{
The following add-ins are available:
\itemize{
    \item \code{Str}:  \cr run \code{\link[DescTools]{Str}()} on the current selection;
    \item \code{Abstract}:  \cr run \code{\link[DescTools]{Abstract}()} (a less technical description than \code{str()}, meant for using with data.frames) on the current selection
    \item \code{Head}:  \cr run R-command \code{\link{head}()} on the current selection
    \item \code{Some}:  \cr run DescTools command \code{\link[DescTools]{Some}()} on the current selection
    \item \code{Desc}:  \cr run \code{\link[DescTools]{Desc}()} on the current selection
    \item \code{Summary}:  \cr run R-command \code{\link{summary}()} on the current selection
    \item \code{Example}:  \cr run R-command \code{example()} on the current selection
    \item \code{Select}:  \cr select or pick things interactively. This function as well evaluates the current selection. If the selection corresponds to \code{pch} a dialog for selecting point characters will be displayed. If it's \code{col} the \code{\link{ColPicker}()} dialog will be displayed. If it's any of \code{path}, \code{fn}, \code{file} the \code{FileOpen()} dialog will be displayed. In all other case the \code{\link[DescTools]{SelectVarDlg}()} on the current selection will be run. This can be used to select variablenames of a \code{data.frame} by mouse click or the levels of a factor (all depending on the class of the selected object). The variable names will be returned as string, enquoted and separated by a comma.
   \item \code{BuildModel}:  \cr display dialog for building up a model formula interactively by point and click.
    \item \code{Plot}:  \cr run \code{\link{plot}()} on the current selection
    \item \code{PlotD}:  \cr run \code{plot(Desc())} on the current selection
    \item \code{Save}:  \cr display a file dialog, get the desired path and filename and save the selected object in the given place.
    \item \code{XLView}:  \cr view selected object in MS-Excel, which makes sense for data.frames.
    \item \code{IntView}:  \cr view selected object with internal viewer (here again: only data.frames)
    \item \code{FileOpen}:  \cr get the path of a file to open and insert in code. (See also: \code{Select()})
     \item \code{FlipBackSlash}: \cr Handling backslashes in pathnames can be really annoying on Windows. This function replaces all backslashes in the selected text by simple slashes. It's cool to set this command on: Ctrl+Shift+/
    \item \code{SetArrow}:  \cr place an arrow in a plot and insert code into source file
    \item \code{Enquote}:  \cr split the selected text in the source panel using line breaks as separators, enquote the sections and paste them separated by comma.
    \item \code{EvalEnquote}:  \cr evaluates the selection before enquoting the parts and paste them separated by comma. This can be used for enquote factors.
    \item \code{Edit}:  \cr run R-command \code{\link{fix}()} on the current selection. This is nice for editing data frames interactively.
    \item \code{NewObject}:  \cr starts the good old internal editor and allows to enter values. In dependence of the initial selection in the code panel different objects will be returned. If the selection was \code{m} the code for a matrix will be created, if the selection is \code{d} then the code for a data.frame will be returned and a c will be interpreted as vector. Default is \code{m}. Note that column names can be defined within the editor, but not rownames (don't ask me why ...).
    \item \code{InspectPnt}:  \cr Lauch \code{IdentifyA(... , poly=TRUE)} with the current selection as argument and returns the selected points on the plot.
    \item \code{Unclass}:  \cr run R-command \code{\link{unclass}()} on the current selection
    \item \code{GetXLRange}:  \cr run DescTools command \code{\link[DescTools]{XLGetRange}()} by taking the current selection as name for the imported data and replace the current selection by the used code.
    \item \code{Class}: \cr run \code{class} on the current selection.
  }

As soon as the package is loaded once, the AddIns will be listed in RStudios AddIns listbox:\cr\cr
\figure{AddIns.png}{AddIns listbox}

The keyboard shortcuts can then be assigned by clicking on the menu \code{Tools | Modify Keyboard Shortcuts...} and scroll down to the Addin scope:\cr\cr
\figure{Shortcuts.png}{Keyboard Shortcuts}

}
\author{Andri Signorell <andri@signorell.net>
%%  ~~who you are~~
}
\examples{
if (interactive()){
  Str()
}
}
\keyword{ utilities }
