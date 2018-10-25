{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LatexPreamble
    ( preamble
    , ending
    )
where

import Core.Text

preamble :: Rope
preamble = [quote|
\documentclass[12pt,a4paper,openany]{memoir}

%
% Load the TeX Gyre project's "Heros" font, which is an upgrade of URW's
% lovely "Nimbus Sans L" sans-serif font.
%

\usepackage{fontspec}
\setmainfont{Linux Libertine O}
\setsansfont{TeX Gyre Heros}[Scale=MatchLowercase]
\setmonofont{Inconsolata}[Scale=MatchLowercase]

%\usepackage[showframe, pass]{geometry}

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

\SingleSpacing
\traditionalparskip
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

\begin{document}
|]

ending :: Rope
ending = [quote|
\end{document}
|]
