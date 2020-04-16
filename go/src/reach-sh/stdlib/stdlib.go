package stdlib

type Type_bool bool
type Type_uint256 int
type Type_address int

type Txn struct { 
     Balance Type_uint256
     Value Type_uint256
     DidTimeout bool
     From Type_address
     Data Msg }

var Txn0 = Txn{ Balance: 0, Value: 0 }

func Ite_uint256(c bool, x Type_uint256, y Type_uint256) Type_uint256 {
  if c {
    return x
  } else {
    return y } }

func Ite_bool(c bool, x Type_bool, y Type_bool) Type_bool {
  if c {
    return x
  } else {
    return y } }

func Eq(x Type_uint256, y Type_uint256) bool {
  return x == y }

func Add(x Type_uint256, y Type_uint256) Type_uint256 {
  return x + y }

func Sub(x Type_uint256, y Type_uint256) Type_uint256 {
  return x - y }

func Div(x Type_uint256, y Type_uint256) Type_uint256 {
  return x / y }

func Mod(x Type_uint256, y Type_uint256) Type_uint256 {
  return x % y }

func Gt(x Type_uint256, y Type_uint256) bool {
  return x > y }

func Lt(x Type_uint256, y Type_uint256) bool {
  return x < y }

func Keccak256(x Type_uint256, y Type_uint256) Type_uint256 {
  panic("XXX") }

func Assert( b bool ) {
  if b {
    return
  } else {
    panic("Assertion failed") } }

type Msg int

var Msg0 Msg = 0

func MsgEncode_uint256( m Msg, v Type_uint256 ) Msg {
  panic("XXX") }

func MsgEncode_uint256arr( m Msg, v []Type_uint256 ) Msg {
  panic("XXX") }

func MsgEncode_address( m Msg, v Type_address ) Msg {
  panic("XXX") }

func MsgDecode_bool( m Msg, path []string ) bool {
  panic("XXX") }

type Contract int

func (c Contract) SendRecv(dbg string, m_name string, m Msg, amount Type_uint256, e_name string, delay Type_uint256, t_name string ) <-chan Txn {
  panic("XXX SendRecv")
}

func (c Contract) Recv(dbg string, e_name string, delay Type_uint256, to_me bool, to_m Msg, to_m_name string, to_e_name string ) <-chan Txn {
  panic("XXX Recv")
}
