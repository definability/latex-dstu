\usepackage{minted}

\makeatletter
\newcommand{\minted@write@detok}[1]{%
  \immediate\write\FV@OutFile{\detokenize{#1}}}%

\newcommand{\minted@FVB@VerbatimOut}[1]{%
  \@bsphack
  \begingroup
    \FV@UseKeyValues
    \FV@DefineWhiteSpace
    \def\FV@Space{\space}%
    \FV@DefineTabOut
    \let\FV@ProcessLine\minted@write@detok %Patch for Unicode
    \immediate\openout\FV@OutFile #1\relax
    \let\FV@FontScanPrep\relax
    \let\@noligs\relax
    \FV@Scan}
    \let\FVB@VerbatimOut\minted@FVB@VerbatimOut

\renewcommand\minted@savecode[1]{
  \immediate\openout\minted@code\jobname.pyg
  \immediate\write\minted@code{\expandafter\detokenize\expandafter{#1}}%
  \immediate\closeout\minted@code}
\makeatother

% центрирование листингов minted в картинках
\RecustomVerbatimEnvironment{Verbatim}{BVerbatim}{}