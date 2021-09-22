cd /dbg
for i in $(ls | grep [[:digit:]]-  | awk -F- '{print $1}' | uniq); do 
  in_use=$(lsof | grep $i-[[:digit:]] ; echo $?)
  if [ $in_use -ne 0 ]; then
    mkdir $i;
    mv $i-* $i;
    tar -czf $i.tgz $i;
    rm -rf $i;
  fi
done