
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{byuthesis}[2021/11/01 BYU Tech MS/PhD Thesis Class]

% based on report class, so pass options to it
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\ProcessOptions\relax
\LoadClass{report}

% package imports - add your own in preamble to the document file with \usepackage{}
\RequirePackage{graphicx}  % for images and plots
\RequirePackage[letterpaper, left=1in, right=1in, top=1in, bottom=1in]{geometry}
\RequirePackage{multicol} % for multicolumn environments
\RequirePackage{setspace}  % use this package to set linespacing as desired
\RequirePackage{newtxtext} % use times new roman font
\RequirePackage{newtxmath}
\RequirePackage{booktabs}
\RequirePackage{indentfirst}
\RequirePackage[explicit]{titlesec}  % title control and formatting
\RequirePackage[titles]{tocloft}  % table of contents control and formatting
\RequirePackage{ifoddpage} % logical page parity checking, used with rotating
\RequirePackage{rotating}  % for rotated, landscape images
\RequirePackage{appendix}  % for appendices
\RequirePackage[normalem]{ulem}  % for underlining text
\RequirePackage{url} % in case explicit URL's are needed
\RequirePackage{siunitx} % several functions related to expressing units
\RequirePackage[bookmarks=true,hidelinks]{hyperref} % should be last(ish)
\RequirePackage{bookmark} % allows reformatting of pdf bookmarks
\RequirePackage{caption} % captions for figures
\RequirePackage{subcaption} % allows subfigures in a reasonable way, plays nice with caption
\RequirePackage[acronym,nogroupskip,nopostdot]{glossaries} % only for list of acronyms
\RequirePackage{doi} % and this is laster, makes easier hyperlinking for doi sources
\RequirePackage{emptypage} % keeps page numbers off empty pages

\parindent=0.25in

\sisetup{
    sticky-per=true,
    detect-mode=true,
    per-mode = symbol-or-fraction
}

\doublespacing  % set line spacing
\makeglossaries % process glossaries
\urlstyle{same} % uses the text font, not a monospace one, for URLs
\hbadness=9000 % suppress some hbox warnings

% hyperlink coloring, or lack thereof
\hypersetup{
    colorlinks,
    linkcolor={black},
    citecolor={black},
    urlcolor={black}
}

\makeatletter

% formatting of chapter numbers in bookmarks
\bookmarksetup{%
    depth=1, % bookmark depth - 1 goes to section depth, 2 to subsection, etc.
    addtohook={%
    \ifnum 0=\bookmarkget{level}\relax
      \renewcommand*{\numberline}[1]{#1 | }%
    \fi
    \ifnum 0<\bookmarkget{level}\relax
      \renewcommand*{\numberline}[1]{}%
    \fi
    }
}

% so that using ref with algorithms works correctly
\newcommand{\ALC@uniqueautorefname}{Line}
\newcommand{\algorithmautorefname}{Algorithm}

% set up rotatepage environment - this will make landscape figures with awareness of page parity and whether the document is set up as single- or two-sided
\@ifclasswith{report}{twoside}{%
    \newenvironment{rotatepage}{%
        \checkoddpage\ifoddpage
            \clearpage\pagebreak[4]\global\pdfpageattr\expandafter{\the\pdfpageattr/Rotate 270}%
        \else
            \clearpage\pagebreak[4]\global\pdfpageattr\expandafter{\the\pdfpageattr/Rotate 90}%
        \fi
        }%
        {\clearpage\pagebreak[4]\global\pdfpageattr\expandafter{\the\pdfpageattr/Rotate 0}}%
    }{
    \newenvironment{rotatepage}%
        {\clearpage\pagebreak[4]\global\pdfpageattr\expandafter{\the\pdfpageattr/Rotate 90}}%
        {\clearpage\pagebreak[4]\global\pdfpageattr\expandafter{\the\pdfpageattr/Rotate 0}}%
    }

% tokens for author info
\newcommand{\doctype}[1]{\gdef\@doctype{#1}}
\newcommand{\copyyear}[1]{\gdef\@copyyear{#1}}
\newcommand{\degree}[1]{\gdef\@degree{#1}}
\newcommand{\keywords}[1]{\gdef\@keywords{#1}}
\newcommand{\committeeMembers}[1]{\gdef\@committeeMembers{#1}}
\newcommand{\approvaldate}[1]{\gdef\@approvaldate{#1}}%
\newcommand{\@approvaldate}{\@latex@warning@no@line{No \noexpand\approvaldate given}}
\newcommand{\department}[1]{\gdef\@department{#1}}%
\newcommand{\@department}{\@latex@warning@no@line{No \noexpand\department given}}
\makeatother

% frontmatter environment, only handles title formatting
\newenvironment{frontmatter}{%
    \cleardoublepage
        \titleformat{\chapter}[display]{\normalfont\filcenter}{\chaptertitlename\ \thechapter}{0pt}{\MakeUppercase{##1}}
    \titlespacing*{\chapter}
      {0pt}{0pt}{30pt}	% controls vertical margins on title
}%
{%
    \clearpage
}

\newcommand{\thesisbody}{
    \cleardoublepage
    \pagestyle{plain}
    \pagenumbering{arabic} % resumes page numbering after front matter
    \setcounter{page}{1} % set the page number appropriately
    \titleformat{\chapter}{\normalfont\bfseries}{\thechapter}{1em}{\MakeUppercase{##1}}  % spacing between titles
    \titlespacing*{\chapter}
      {0in}{1in}{1in}	% controls vertical margins on title
    \titleformat{\section}{\normalfont\bfseries}{\thesection}{1em}{##1}
    \titleformat{\subsection}{\normalfont\bfseries}{\thesubsection}{1em}{##1}
    \titleformat{\subsubsection}{\normalfont\itshape}{\thesubsection}{1em}{##1}

}

\makeatletter
\renewcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \gdef\@chapapp{\appendixname}%
  \gdef\thechapter{\@Alph\c@chapter}
  \titleformat{\chapter}{\normalfont\bfseries}{APPENDIX \thechapter}{1em}{\MakeUppercase{##1}}  % spacing between titles
}
\makeatother



% ------------------ Title Page ------------------
\makeatletter
% args:{year}
\renewenvironment{titlepage} {
    \phantomsection
    \begin{center}
    \@title\\
    \begin{singlespacing}
    \vfill
    \@author\\
    \vfill
    \addcontentsline{toc}{chapter}{Title Page}
    A \@doctype \ submitted to the faculty of\\
    Brigham Young University\\
    in partial fulfillment of the requirements for the degree of\\
    \vspace{1\baselineskip}
    \@degree\\
    \vfill
    \@committeeMembers\\
    \vfill
    \@department\\
    \vspace{\baselineskip}
    Brigham Young University\\
    \vfill
    Copyright \copyright \ \@copyyear \ \@author \\
    \vspace{1\baselineskip}
    All Rights Reserved
    \end{singlespacing}
    \end{center}
}{}


% ------------------- Abstract Page ------------------
\renewenvironment{abstract}{%
  \thispagestyle{empty}
  \pagenumbering{roman}
  \titlepage
  \cleardoublepage
  \thispagestyle{empty}
  \begin{singlespacing}
  \setlength{\parskip}{1em}
  \begin{center}
    \phantomsection
    \addcontentsline{toc}{chapter}{Abstract}
    \MakeUppercase{abstract}\\
    \vspace{12pt}
    \@title\\
    \vspace{12pt}
    \@author\\
    \@department, BYU\\
    \@degree \end{center}
    \vspace{-12pt}\hspace{0.2in}
}{
  \vfill
  \noindent Keywords: \@keywords
  \end{singlespacing}
  \setlength{\parskip}{0em}
  \thispagestyle{empty}
}


\makeatother

% ------------------ Acknowledgments environment/formatting ------------------
\newenvironment{acknowledgments}
{
    \thispagestyle{empty}
    \cleardoublepage
    \thispagestyle{empty}
    \begin{centering}
    \phantomsection
    \addcontentsline{toc}{chapter}{Acknowledgments}
    ACKNOWLEDGMENTS\\
    \vspace{\baselineskip}
    \end{centering}
}
{
    \clearpage
}


% --------------- table of contents formatting and command creation ---------------
\renewcommand{\contentsname}{TABLE OF CONTENTS}
\renewcommand{\cftchapfont}{\normalfont}
\renewcommand{\cftchappagefont}{\normalfont}
\renewcommand{\cftchapdotsep}{\cftdotsep}  % add dot separators
\renewcommand{\cftbeforechapskip}{0pt}
\renewcommand\cftdotsep{2}
\renewcommand{\cftchapleader}{\cftdotfill{\cftchapdotsep}}
\newcommand{\makeTOC}{
    % format title font size and position (this also applies to list of figures and list of tables)
    \phantomsection
    \addcontentsline{toc}{chapter}{Table of Contents}
    \tableofcontents
}

% ------------------ List of Tables ------------------
\newcommand{\makeListOfTables}{
\cleardoublepage
\begin{singlespace}
\setlength\cftbeforetabskip{\baselineskip}  % manually set spacing between entries
\phantomsection
\addcontentsline{toc}{chapter}{List of Tables}
\listoftables
\end{singlespace}
\clearpage
}

% ------------------ List of Figures ------------------
\newcommand{\makeListOfFigures}{
\cleardoublepage
\begin{singlespace}
\setlength\cftbeforefigskip{\baselineskip}  % manually set spacing between entries
\phantomsection
\addcontentsline{toc}{chapter}{List of Figures}
\listoffigures
\end{singlespace}
\clearpage
}

% ------------------ List of Acronyms ------------------
\newcommand{\makeListOfAcronyms}{
\cleardoublepage
\singlespacing
\phantomsection
\addcontentsline{toc}{chapter}{LIST OF ACRONYMS}
\printacronyms[nonumberlist,title={List of Acronyms}]
\doublespacing
}


% ------------------ Do all FrontMatter ------------------
\newcommand{\makefrontmatter}{
  \begin{frontmatter}
  \makeTOC
  \makeListOfTables
  \makeListOfFigures
  %\makeListOfAcronyms
  \end{frontmatter}
}


% ------------------ appendix environment definition ------------------
% A note on layout: in the Chicago (and by extension IEEE) style manual, appendices are first in the end matter, then bibliography or references. In APA, bibliography is first, then appendices. The thesis manual on which this template is based assumes the Chicago convention.

% remove the page number on the appendices cover and move bookmark up the page, create a group-subgroup structure for the appendices in the TOC/bookmarks
\makeatletter
\newenvironment{theappendices}{
    \bookmarksetup{depth=0,open}
    \addtocontents{toc}{\protect\setcounter{tocdepth}{1}}
    \bookmarksetupnext{level=part}
    \begin{appendices}
    \titleformat{\chapter}{\normalfont\bfseries}{\thechapter}{1em}{\MakeUppercase{##1}}
    % spacing between titles \titlespacing*{\chapter} {0.25in}{1in}{1in}
    % controls vertical margins on title
    \addtocontents{toc}{\protect\renewcommand{\protect\cftchappresnum}{\appendixname\space}}
    \addtocontents{toc}{\protect\renewcommand{\protect\cftchapnumwidth}{5.75em}}
    \addtocontents{toc}{\protect\renewcommand{\protect\cftsecaftersnum}{\space}}
    \addtocontents{toc}{%
    \begingroup
    }
}{
    \addtocontents{toc}{\endgroup}
    \end{appendices}
}
\makeatother

% ------------------ bibliography formatting and command creation ------------------

% I am only leaving these \AtEveryBibitem lines in because they were in the original template when I downloaded it. Change these at your own leisure.
% ------------------ vita environment ------------------
\newenvironment{vita}{
    \bookmarksetupnext{level=part}
    \chapter*{Vita}
    \addcontentsline{toc}{chapter}{Vita}
}{
}
