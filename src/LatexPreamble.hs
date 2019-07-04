{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LatexPreamble
    ( preamble
    , beginning
    , ending
    )
where

import Core.Text

preamble :: Rope
preamble = [quote|
\documentclass[12pt,a4paper,oneside,openany]{memoir}

%
% Load the TeX Gyre project's "Heros" font, which is an upgrade of URW's
% lovely "Nimbus Sans L" sans-serif font.
%

\usepackage{fontspec}
\setmainfont{Linux Libertine O}
\setsansfont{TeX Gyre Heros}[Scale=MatchLowercase]
\setmonofont{Inconsolata}[Scale=MatchLowercase]

% use upquote for straight quotes in verbatim environments
\usepackage{upquote}

% use microtype
\usepackage{microtype}
\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts

%
% Customize paper size. Or not: A4 paper is 597pt x 845pt. 4:3 aka 768x1024
% screen is 597pt x 796pt, but 16:9 aka 2560x1440 screen is 597pt x 1062pt. A4
% in landscape is a fair way narrower.
%

\setlrmarginsandblock{2cm}{2.5cm}{*}
\setulmarginsandblock{2cm}{2cm}{*}

%
% Setting the \footskip parameter is how you control the bottom margin width,
% not "setting the bottom margin" since the typeblock will be set to be an
% integer multiple of \baselineskip.
%

\setheadfoot{0pt}{25pt}
\setheaderspaces{1cm}{*}{*}

\checkandfixthelayout[classic]

\usepackage{graphicx,grffile}

\usepackage{longtable}

\setlength{\emergencystretch}{3em}  % prevent overfull lines

\usepackage[hidelinks]{hyperref}

%
% Get rid of default headers and put page number in footer.
%

\makeoddfoot{plain}{}{}{\tiny\textsf{\thepage/\thelastpage}}
\makeevenfoot{plain}{\tiny\textsf{\thepage/\thelastpage}}{}{}

\makeoddhead{plain}{}{}{}
\makeevenhead{plain}{}{}{}

\pagestyle{plain}

\SingleSpacing
\nonzeroparskip
\setlength{\parindent}{0em}

%
% Customize the section heading fonts to use this accordingly.
%

\chapterstyle{article}
\setsecnumdepth{none}

% FIXME Why isn't the \Huge font size command working?
\renewcommand{\chaptitlefont}{\Large\sffamily\bfseries}

\setsecheadstyle{\large\sffamily}
\setsubsecheadstyle{\normalsize\sffamily\bfseries}
\setsubsubsecheadstyle{\normalsize\rmfamily\itshape}

|]

beginning :: Rope
beginning = [quote|

%
% Output from Skylighting.styleToLaTeX
%

\usepackage{color}
\usepackage{fancyvrb}
\newcommand{\VerbBar}{|}
\newcommand{\VERB}{\Verb[commandchars=\\\{\},samepage=true]}
\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\{\}}
% Add ',fontsize=\small' for more characters per line
\usepackage{framed}
\definecolor{shadecolor}{RGB}{248,248,248}
\newenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
\newcommand{\AlertTok}[1]{\textcolor[rgb]{0.94,0.16,0.16}{#1}}
\newcommand{\AnnotationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{#1}}}}
\newcommand{\AttributeTok}[1]{\textcolor[rgb]{0.77,0.63,0.00}{#1}}
\newcommand{\BaseNTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{#1}}
\newcommand{\BuiltInTok}[1]{#1}
\newcommand{\CharTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{#1}}
\newcommand{\CommentTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textit{#1}}}
\newcommand{\CommentVarTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{#1}}}}
\newcommand{\ConstantTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{#1}}
\newcommand{\ControlFlowTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{\textbf{#1}}}
\newcommand{\DataTypeTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{#1}}
\newcommand{\DecValTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{#1}}
\newcommand{\DocumentationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{#1}}}}
\newcommand{\ErrorTok}[1]{\textcolor[rgb]{0.64,0.00,0.00}{\textbf{#1}}}
\newcommand{\ExtensionTok}[1]{#1}
\newcommand{\FloatTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{#1}}
\newcommand{\FunctionTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{#1}}
\newcommand{\ImportTok}[1]{#1}
\newcommand{\InformationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{#1}}}}
\newcommand{\KeywordTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{\textbf{#1}}}
\newcommand{\NormalTok}[1]{#1}
\newcommand{\OperatorTok}[1]{\textcolor[rgb]{0.81,0.36,0.00}{\textbf{#1}}}
\newcommand{\OtherTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{#1}}
\newcommand{\PreprocessorTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textit{#1}}}
\newcommand{\RegionMarkerTok}[1]{#1}
\newcommand{\SpecialCharTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{#1}}
\newcommand{\SpecialStringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{#1}}
\newcommand{\StringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{#1}}
\newcommand{\VariableTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{#1}}
\newcommand{\VerbatimStringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{#1}}
\newcommand{\WarningTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{#1}}}}

%
% avoid problems with \sout in headers with hyperref:
%

\usepackage[normalem]{ulem}
\pdfstringdefDisableCommands{\renewcommand{\sout}{}}

\begin{document}
|]


ending :: Rope
ending = [quote|
\end{document}
|]
