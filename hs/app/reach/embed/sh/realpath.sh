# OS X doesn't come with realpath, so we use
# https://github.com/mkropat/sh-realpath/blob/master/realpath.sh
# if we can't find realpath

command -v realpath >/dev/null 2>&1 || realpath() {
  set +e
  r="$(canonicalize_path "$(resolve_symlinks "$1")")"
  set -e
  echo "$r"
}

resolve_symlinks() {
  _resolve_symlinks "$1"
}

_resolve_symlinks() (
  _assert_no_path_cycles "$@" || return

  path=$(readlink -- "$1")
  res=$?
  if [ "$res" -eq 0 ]; then
    dir_context=$(dirname -- "$1")
    _resolve_symlinks "$(_prepend_dir_context_if_necessary "$dir_context" "$path")" "$@"
  else
    printf '%s\n' "$1"
  fi
)

_prepend_dir_context_if_necessary() {
  if [ "$1" = . ]; then
    printf '%s\n' "$2"
  else
    _prepend_path_if_relative "$1" "$2"
  fi
}

_prepend_path_if_relative() {
  case "$2" in
    /* ) printf '%s\n' "$2" ;;
     * ) printf '%s\n' "$1/$2" ;;
  esac
}

_assert_no_path_cycles() (
  target=$1
  shift

  for path in "$@"; do
    if [ "$path" = "$target" ]; then
      return 1
    fi
  done
)

canonicalize_path() {
  if [ -d "$1" ]; then
    _canonicalize_dir_path "$1"
  else
    _canonicalize_file_path "$1"
  fi
}

_canonicalize_dir_path() {
  (cd "$1" 2>/dev/null && pwd -P)
}

_canonicalize_file_path() (
  dir=$(dirname -- "$1")
  file=$(basename -- "$1")
  (cd "$dir" 2>/dev/null && printf '%s/%s\n' "$(pwd -P)" "$file")
)
