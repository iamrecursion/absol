
add_cus_dep( 'glo', 'gls', 0, 'makeglossaries' );
sub makeglossaries {
    system( "makeglossaries \"$_[0]\"" );
}

$latex = 'latex -interaction=nonstopmode -shell-escape';
$pdflatex = 'pdflatex -interaction=nonstopmode -shell-escape';
$clean_ext .= " %R.loa %R.lol %R.acn %R.acr %R.alg %R.glg %R.glo %R.gls %R.xdy synctex.gz synctex.gz(busy) run.xml tex.bak bbl bcf fdb_latexmk run tdo %R-blx.bib"
