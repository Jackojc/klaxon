# Klaxon Specification

### Arithmetic
	Two's complement signed arithmetic
	Word size integers

### Source Encoding
	UTF-8 with no BOM

### Compiler Flags


### Stack
	Implementation defined size

### Core Words
	remove
	copy
	move

### Implementation Defined Words
	add sub mul div mod lsh rsh
	lt gt eq
	and or not
	band bor bnot
	word
	byte
	load
	store

### Guaranteed Optimisations
	Inlining
	Constant folding

### Standard Library


### Style Guide
	Snake case
	Tabs for indentation
	80 columns
	OVS style naming. i.e: size-str/size-vec vs str-size/vec-size
	Kebab case
