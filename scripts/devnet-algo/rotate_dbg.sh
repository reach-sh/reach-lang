#!/bin/sh
HOW_MANY="$1"
for i in $(find . -name '*.json' | sed 'sX^./XX' | awk -F- '{print $1}' | sort -n | uniq | head -n -"${HOW_MANY}") ; do
  rm -f "${i}"-*
done

# XXX I observe about 150mb with HOW_MANY=16; if that's too much, then we could
# compress also:
#
#  in_use=$(lsof | grep $i-[[:digit:]] ; echo $?)
#  if [ $in_use -ne 0 ]; then
#    mkdir $i;
#    mv $i-* $i;
#    tar -czf $i.tgz $i;
#    rm -rf $i;
#  fi
