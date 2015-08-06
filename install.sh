#!/usr/bin/env sh

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

rm -rf ${HOME}/.emacs.d
rm -f ${HOME}/.emacs

ln -s ${DIR}/.emacs.d ${HOME}
