#!/bin/sh

# script por Mikael Lemos
# a shell script used to download a specific url. 

#DIR=\Users\mikael.lemos\Desktop\TABWIN\APAC

# wget log file

LOGFILE=wget.log

# wget download url

URL=ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/AM*

# Entrar na pasta

#cd $DIR

# wget download

echo "Baixando as tabelas"

wget -r -l0 $URL -o $LOGFILE

echo "Processo finalizado ;-)"