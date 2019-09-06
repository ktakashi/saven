#!/bin/sh

PREFIX=${INSTALL_PREFIX:-/usr/local}
BIN_DIR=${PREFIX}/bin
LIB_DIR=${PREFIX}/share/sav
MAN_DIR=${PREFIX}/share/man/man1

ARCHIVE_URL=https://github.com/ktakashi/saven/archive/master.zip
SAV_DIR_NAME=saven-master

WORK_DIR=$(mktemp -d)

curl -sL ${ARCHIVE_URL} -o ${WORK_DIR}/master.zip
unzip ${WORK_DIR}/master.zip -d ${WORK_DIR}

cat ${WORK_DIR}/saven-master/bin/sav \
    | sed "s|exec sagittarius|exec sagittarius -L${LIB_DIR}|" > ${BIN_DIR}/sav
chmod +x ${BIN_DIR}/sav

if [ -d ${LIB_DIR} ]; then
    rm -rf ${LIB_DIR}
fi
mv ${WORK_DIR}/saven-master/lib/ ${LIB_DIR}

cp ${WORK_DIR}/saven-master/doc/sav.1 ${MAN_DIR}

echo Cleaning up
rm -rf ${WORK_DIR}

echo Done!
