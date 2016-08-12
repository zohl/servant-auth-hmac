BUILD_DIR="$(pwd)/result/static";
SOURCE_DIR="./src"

pushd "$SOURCE_DIR"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR/tmp"

tsc --outDir "$BUILD_DIR/tmp" --noEmitOnError --target ES6 *.ts
rollup -o "$BUILD_DIR/app.js" "$BUILD_DIR/tmp/app.js"

rm -r "$BUILD_DIR/tmp"

popd
