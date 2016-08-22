CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$CURRENT_DIR/result/static";
SOURCE_DIR="$CURRENT_DIR/src"

set -ev

pushd "$SOURCE_DIR"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR/tmp"

tsc --outDir "$BUILD_DIR/tmp" --noEmitOnError --target ES6 *.ts
rollup -o "$BUILD_DIR/app.js" "$BUILD_DIR/tmp/app.js"

rm -r "$BUILD_DIR/tmp"

popd
