%----------------------------------------------------------------------
% Stylish Article
% Original....: Mathias Legrand (legrand.mathias@gmail.com)
% Modificado..: Fernando Anselmo
%----------------------------------------------------------------------

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SelfArx}[25/01/2012, v1.0]
\RequirePackage{ifthen}
\RequirePackage{calc}
\AtEndOfClass{\RequirePackage{microtype}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions*
\LoadClass{article}
\RequirePackage{ifpdf}      % Needed to pick between latex and pdflatex

%----------------------------------------------------------------------
%	FONTS
%----------------------------------------------------------------------
\RequirePackage{times}      % Loads the Times-Roman Fonts
\RequirePackage{mathptmx}   % Loads the Times-Roman Math Fonts

%----------------------------------------------------------------------
%	VARIOUS USEFUL PACKAGES
%----------------------------------------------------------------------
\RequirePackage[utf8]{inputenc}
\RequirePackage{amsmath,amsfonts,amssymb}
\RequirePackage{graphicx,xcolor}
\RequirePackage{booktabs}

%----------------------------------------------------------------------
%	MARGINS	
%----------------------------------------------------------------------
\RequirePackage[a4paper, inner=1.5cm, outer=2cm, top=2cm, bottom=3cm, 
bindingoffset=1cm]{geometry} %

%----------------------------------------------------------------------
%	FIGURES AND TABLES CAPTIONS
%----------------------------------------------------------------------
\RequirePackage[labelfont={bf,sf,small},%
  labelsep=period,%
  justification=raggedright]{caption}
\setlength{\abovecaptionskip}{0pt}
\setlength{\belowcaptionskip}{0pt}
				
%----------------------------------------------------------------------
%	PAGE HEADER
%----------------------------------------------------------------------
\RequirePackage{fancyhdr}  % Needed to define custom headers/footers
\RequirePackage{lastpage}  % Number of pages in the document
\pagestyle{fancy}          % Enables the custom headers/footers

% Headers
\lhead{}%
\chead{}%
\rhead{\small\sffamily\bfseries\@PaperTitle\  --- \thepage/\pageref{LastPage}}

% Footers
\lfoot{}%
\cfoot{}%
\rfoot{}%
\renewcommand{\headrulewidth}{0pt}% % No header rule
\renewcommand{\footrulewidth}{0pt}% % No footer rule

%----------------------------------------------------------------------
%	SECTION/SUBSECTION/PARAGRAPH SET-UP
%----------------------------------------------------------------------
\RequirePackage[explicit]{titlesec}
\titleformat{\section}
  {\color{color1}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{color2!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering\arabic{section}. #1}}}
  []
\titleformat{name=\section,numberless}
  {\color{color1}\large\sffamily\bfseries}
  {}
  {0em}
  {\colorbox{color2!10}{\parbox{\dimexpr\linewidth-2\fboxsep\relax}{\centering#1}}}
  []  
\titleformat{\subsection}
  {\color{color1}\sffamily\bfseries}
  {\thesubsection}
  {0.5em}
  {#1}
  []
\titleformat{\subsubsection}
  {\sffamily\small\bfseries}
  {\thesubsubsection}
  {0.5em}
  {#1}
  []    
\titleformat{\paragraph}[runin]
  {\sffamily\small\bfseries}
  {}
  {0em}
  {#1} 
\titlespacing*{\section}{0pc}{3ex \@plus4pt \@minus3pt}{5pt}
\titlespacing*{\subsection}{0pc}{2.5ex \@plus3pt \@minus2pt}{0pt}
\titlespacing*{\subsubsection}{0pc}{2ex \@plus2.5pt \@minus1.5pt}{0pt}
\titlespacing*{\paragraph}{0pc}{1.5ex \@plus2pt \@minus1pt}{10pt}

%----------------------------------------------------------------------
%	TABLEOFCONTENTS SET-UP
%----------------------------------------------------------------------
\newlength{\tocsep} 
\setlength\tocsep{1.5pc} % Sets the indentation of the sections in the table of contents
\setcounter{tocdepth}{3} % Three levels in the table of contents section: sections, subsections and subsubsections

\usepackage{titletoc}
\contentsmargin{0cm}
\titlecontents{section}[\tocsep]
  {\addvspace{4pt}\small\bfseries\sffamily}
  {\contentslabel[\thecontentslabel]{\tocsep}}
  {}
  {\hfill\thecontentspage}
  []
\titlecontents{subsection}[\tocsep]
  {\addvspace{2pt}\sffamily}
  {\contentslabel[\thecontentslabel]{\tocsep}}
  {}
  {\ \titlerule*[.5pc]{.}\ \thecontentspage}
  []
\titlecontents*{subsubsection}[\tocsep]
  {\footnotesize\sffamily}
  {}
  {}
  {}
  [\ \textbullet\ ]  

%----------------------------------------------------------------------
%	MULTIPLE AUTHOR SET
%----------------------------------------------------------------------  
\newcount\@authcnt
\newcount\@tmpcnt\@tmpcnt\z@

\def\@affiliation{%
  \ifnum\@tmpcnt<\@authcnt
   \global\advance\@tmpcnt1
    \raggedright \csname @auth\romannumeral\the\@tmpcnt\endcsname\hfill\\%
   \let\next\@affiliation \vskip1pt
  \else
   \let\next\relax
  \fi
\next}
	 
\newcommand{\affiliation}[1]{%
    \global\advance\@authcnt1
    \expandafter\gdef\csname @auth\romannumeral\the\@authcnt\endcsname
    {#1}}

%----------------------------------------------------------------------
%	LIST CONTROL
%----------------------------------------------------------------------  
\RequirePackage{enumitem}
%\setlist{nolistsep} % Uncomment to remove spacing between items in lists (enumerate, itemize)

%----------------------------------------------------------------------
%	ABSTRACT+AUTHOR FRAME
%----------------------------------------------------------------------  
\newcommand{\PaperTitle}[1]{\def\@PaperTitle{#1}}
\newcommand{\Archive}[1]{\def\@Archive{#1}}
\newcommand{\Authors}[1]{\def\@Authors{#1}}
\newcommand{\JournalInfo}[1]{\def\@JournalInfo{#1}}
\newcommand{\Abstract}[1]{\def\@Abstract{#1}}
\newcommand{\Keywords}[1]{\def\@Keywords{#1}}

% ---------------------------------------------------------------------

\renewcommand{\@maketitle}{%
\onecolumn{%
	{\raggedright\color{color1}\sffamily\bfseries\fontsize{20}{25}\selectfont \@PaperTitle\par}%
	{\raggedright\color{color1}\sffamily\fontsize{12}{16}\selectfont  \@Authors\par}%
	{\raggedleft\small\sffamily\bfseries\@JournalInfo\\\@Archive\par}%
	\vskip18pt%
	\fcolorbox{color1}{white}{%
	\parbox{\textwidth-2\fboxsep-2\fboxrule}{\centering%
	\colorbox{color2!10}{%
	\parbox{\textwidth-4\fboxsep-2\fboxrule}{%
	\ifx\@Keywords\@empty%
	\sffamily\textbf{\abstractname}\\\@Abstract%
	\else%
	\sffamily\textbf{\abstractname}\\\@Abstract\\[4pt]%
	\textbf{\keywordname}\\\@Keywords%
	\fi%
	}%
	}%
	\vskip4pt%
	\begingroup%
	\raggedright\sffamily\small%
	\footnotesize\@affiliation\par%
	\endgroup%%
	}%
	}%
	\vskip15pt%
	}%
}

%----------------------------------------------------------------------
%	REFERENCES
%----------------------------------------------------------------------  

% Remove brackets from numbering in List of References
\renewcommand{\@biblabel}[1]{\bfseries\color{color1}\textsuperscript{[#1]}}
%\setlength{\bibitemsep}{0cm}
\let\oldbibliography\thebibliography
\renewcommand{\thebibliography}[1]{%
\addcontentsline{toc}{section}{\refname}%
\oldbibliography{#1}%
\setlength\itemsep{0pt}}%

%-----------------------------------------------------------------------------
% Outros Pacotes
%-----------------------------------------------------------------------------
\usepackage[brazil]{babel} % padronizar a linguagem
\usepackage[utf8]{inputenc} % permitir a acentuação
\usepackage[protrusion=true,expansion=true]{microtype} % obter melhor tipografia
\usepackage{fix-cm}   % customizar o tamanho das fontes
\usepackage{sectsty}  % habilitar a customização dos títulos das seções
\usepackage{listings} % listagens
\usepackage{url} % codigo para as URLs
\usepackage{enumerate} % simplicidade para as listas
\usepackage{enumitem} % para usar o noitemsep
\usepackage{nicefrac} % Fração de Linha
\usepackage{float} % Imagens flutuantes
\usepackage[framemethod=TikZ]{mdframed} % janela de ATENÇÃO
\usepackage{array} % Para usar tabelas
\usepackage{wrapfig} % Imagens que se cruzam com texto

%-----------------------------------------------------------------------------
% Definições Iniciais
%-----------------------------------------------------------------------------
% modificar toda a fonte de todas as seções
\allsectionsfont{\usefont{OT1}{phv}{b}{n}}

% Definições para Tabela
\newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{R}[1]{>{\raggedleft\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}

\setlength{\tabcolsep}{18pt}
\renewcommand{\arraystretch}{1.4}

%----------------------------------------------------------------------
% Espaçamento dos Parágrafos
%----------------------------------------------------------------------
\setlength{\parindent}{0em}
\setlength{\parskip}{1em}

%----------------------------------------------------------------------
% Traduções
%----------------------------------------------------------------------
\addto\captionsenglish{ % modificar os nomes do ingles
	\renewcommand{\abstractname}{Resumo}
	\renewcommand{\bibname}{Consulte também...}
	\renewcommand{\contentsname}{Sumário}
	\renewcommand{\listfigurename}{Figuras}
	\renewcommand{\lstlistingname}{Listagem}
	\renewcommand{\lstlistlistingname}{Listagens}
	\renewcommand{\refname}{Referências}
	\renewcommand{\keywordname}{Palavras Chaves}
}

%-----------------------------------------------------------------------------
% Definição para as caixas de listagens
%-----------------------------------------------------------------------------
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}
\lstset {
	aboveskip=3mm,
	backgroundcolor=\color{backcolour},
	basicstyle={\small\ttfamily},
	belowskip=3mm,
	breaklines=true,
	breakatwhitespace=true,
	columns=flexible,
	commentstyle=\textit,
	extendedchars=true,
	frame=tb,
	keepspaces=true,
	keywordstyle=\color{blue}\bfseries,
	language=HTML,
	numbers=left,
	numbersep=5pt,
	numberstyle=\tiny\color{codegray},
	showstringspaces=false,
	showtabs=false,
	tabsize=3,
	literate=%
	{á}{{\'a}}1
	{é}{{\'e}}1
	{í}{{\'i}}1
	{ó}{{\'o}}1
	{ú}{{\'u}}1
	{ê}{{\^e}}1
	{ã}{{\~a}}1
	{õ}{{\~o}}1
	{ç}{{\c{c}}}1
	{Á}{{\'A}}1
	{É}{{\'E}}1
	{Í}{{\'I}}1
	{Ó}{{\'O}}1
	{Ú}{{\'U}}1
	{Ê}{{\^E}}1
	{Ã}{{\~A}}1
	{Õ}{{\~O}}1
	{Ç}{{\c{C}}}1
}


%-----------------------------------------------------------------------------
% ESTILOS DO TEXTO
%-----------------------------------------------------------------------------
\newcommand{\opcmenu}[1]{\color{DarkBlue}\textbf{#1}\color{Black}}
\newcommand{\opcbotao}[1]{\color{DarkRed}\textbf{#1}\color{Black}}
\newcommand{\codigo}[1]{{\ttfamily #1}}
% \newcommand{\aspas}[1]{$\textquotedblright$#1$\textquotedblright$}
\newcommand{\aspas}[1]{"#1"}

%-----------------------------------------------------------------------------
% BLOCO DE ATENÇÃO
%-----------------------------------------------------------------------------

\newenvironment{theo}[2][]{%
	\ifstrempty{#1}%
	% if condition (without title)
	{\mdfsetup{%
			frametitle={%
				\tikz[baseline=(current bounding box.east),outer sep=0pt]%
				\node[anchor=east,rectangle,fill=red!20]%
				{\strut ATENÇÃO};}%
		}%
	}{\mdfsetup{%
			frametitle={%
				\tikz[baseline=(current bounding box.east),outer sep=0pt]%
				\node[anchor=east,rectangle,fill=red!20]%
				{\strut ATENÇÃO:~#1};}%
		}%
	}%
	% Both conditions
	\mdfsetup{%
		innertopmargin=10pt,linecolor=red!20,%
		linewidth=2pt,topline=true,%
		frametitleaboveskip=\dimexpr-\ht\strutbox\relax%
	}%
	\begin{mdframed}[]\relax}{%
	\end{mdframed} \vspace{-3mm}
}