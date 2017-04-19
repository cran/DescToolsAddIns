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
\alias{Save}
\alias{XLView}
\alias{IntView}
\alias{FileOpen}
\alias{FlipBackSlash}
\alias{SetArrow}
\alias{Enquote}
\alias{EvalEnquote}
\alias{BuildModel}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Handful AddIns to Be Used as Shortcuts in RStudio
%%  ~~function to do ... ~~
}
\description{
A simply great new feature in RStudio is the option to define AddIns and bind shortcuts to them.
This package contains AddIns for a few most used functions
in an analysts (at least mine) daily work (like \code{str()}, \code{example()}, \code{plot()}, \code{head()}, \code{view()}, \code{Desc()}).\cr
Most of these functions will get the current selection in RStudios editor window and send the specific command to the console
while instantly executing it.
Assigning shortcuts to these AddIns will spare you quite a few keystrokes and bring a breeze of GUI-interaction into your life.
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
Save()
XLView()
IntView()
FileOpen()
FlipBackSlash()
SetArrow()
Enquote()
EvalEnquote()
}

\details{
The following AddIns are available:
\itemize{
    \item \code{Str}:  \cr run \code{\link[DescTools]{Str}()} on the current selection;
    \item \code{Abstract}:  \cr run \code{\link[DescTools]{Abstract}()} (a less technical description than str, meant for using with data.frames) on the current selection
    \item \code{Head}:  \cr run R-command \code{\link{head}()} on the current selection
    \item \code{Example}:  \cr run R-command example on the current selection
    \item \code{Some}:  \cr run DescTools command \code{\link[DescTools]{Some}()} on the current selection
    \item \code{Desc}:  \cr run \code{\link[DescTools]{Desc}()} on the current selection
    \item \code{Select}:  \cr run \code{\link[DescTools]{SelectVarDlg}()} on the current selection. When the selection is a data.frame, this will display a dialog with the variable names listed, such that they can get selected by point and click. The variable names will be returned as string, enquoted and separated by a comma.
    \item \code{Plot}:  \cr run \code{\link{plot}()} on the current selection
    \item \code{Save}:  \cr display a file dialog, get the desired path and filename and save the selected object in the given place.
    \item \code{XLView}:  \cr view selected object in XL, which makes sense for data.frames.
    \item \code{IntView}:  \cr view selected object with internal viewer (here too: data.frames)
    \item \code{FileOpen}:  \cr get the path of a file to open and insert in code
    \item \code{BuildModel}:  \cr display dialog for building up a model formula.
    \item \code{FlipBackSlash}: \cr Handling backslashes in pathnames can be really annoying on Windows. This function replaces all backslashes in the selected text by simple slashes. It's cool to set this command on: Ctrl+Shift+/
    \item \code{SetArrow}:  \cr place an arrow in a plot and insert code into source file
    \item \code{Summary}:  \cr run \code{\link{summary}())} on the current selection
    \item \code{Enquote}:  \cr split the selected text using line breaks, enquote the parts and paste them separated by comma
    \item \code{EvalEnquote}:  \cr evaluates the selection before enquoting the parts and paste them separated by comma. This can be used for enquoting factors.
  }

As soon as the package is loaded once, the AddIns will be listed in RStudios AddIns listbox:\cr\cr
\figure{AddIns.png}{AddIns listbox}

The keyboard shortcuts can then be assigned by clicking on the specific button.
}
\value{
no values returned
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
