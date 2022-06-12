#!/bin/sh -e
export REACH_DEBUG=0
alias reach="../../reach"

rm -f Seller.in Seller.out Buyer.in Buyer.out
mkfifo Seller.in Seller.out Buyer.in Buyer.out

[ -z "$CIRCLECI" ] && reach devnet --await-background
reach run index seller < Seller.in > Seller.out &
reach run index buyer  < Buyer.in  > Buyer.out  &

exec 3> Seller.in
exec 4< Seller.out
exec 5> Buyer.in
exec 6< Buyer.out
unlink Seller.in
unlink Seller.out
unlink Buyer.in
unlink Buyer.out

read_seller() {
    read -r REPLY <&4
    echo "Seller reads: $REPLY"
}

write_seller() {
    echo "Seller says:  $*"
    echo "$@" >&3
}

read_buyer() {
    read -r REPLY <&6
    echo "Buyer reads:  $REPLY"
}

write_buyer() {
    echo "Buyer says:   $*"
    echo "$@" >&5
}

while ! (echo "$REPLY" | grep -q "Enter a wise phrase"); do read_seller; done
write_seller "Don't let robots take your job!"
while ! (echo "$REPLY" | grep -q "Contract info:"); do read_seller; done
CTC_INFO="$(echo "$REPLY" | awk --field-separator="Contract info: " '{print $2}')"

while [ "$REPLY" != "Paste contract info:" ]; do read_buyer; done
write_buyer "$CTC_INFO"
read_buyer
read_buyer
read_buyer
write_buyer y
read_buyer
read_seller
read_buyer
read_seller
read_seller
read_buyer
read_buyer

wait
