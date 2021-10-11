tabelas: tabelas/tabelas1.tex tabelas/tabelas2.tex tabelas/tabelas3.tex tabelas/tabelas4.tex tabelas/tabelas5.tex
	echo "Tabelas criadas"

tabelas/tabelas1.tex: Main
	echo "Criando tabela1.tex..."
	./Main 1 > tabelas/tabela1.tex

tabelas/tabelas2.tex: Main
	echo "Criando tabela2.tex..."
	./Main 2 > tabelas/tabela2.tex

tabelas/tabelas3.tex: Main
	echo "Criando tabela3.tex..."
	./Main 3 > tabelas/tabela3.tex

tabelas/tabelas4.tex: Main
	echo "Criando tabela4.tex..."
	./Main 4 > tabelas/tabela4.tex

tabelas/tabelas5.tex: Main
	echo "Criando tabela5.tex..."
	./Main 5 > tabelas/tabela5.tex

Main:
	ghc -O2 -Wall -DRELEASE Main.hs
