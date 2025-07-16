# hmake
Reproduction of GNU make with more powerful syntaxes

## Example
```
BUILD_DIR  = "./build"
PRODUCTION = False

build:
    $(mkdir -p $(BUILD_DIR))
    $(cp -v -r static/* $(BUILD_DIR))

    $(cabal v2-build)

    let INPUT_JS  = "$(cabal list-bin MinesweeperHS-exe).jsexe/all.js"
        OUTPUT_JS = "$(BUILD_DIR)/index.js"
    
    if PRODUCTION == True
        then $(node-minify --compressor uglify-js --input "$(INPUT_JS)" --output "$(OUTPUT_JS)")
        else $(cp -v "$(INPUT_JS)" "$(OUTPUT_JS)")
    
    echo ""
    echo "BUILD SUCCESSFUL."
    echo ""

```
