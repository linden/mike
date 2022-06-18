# The MIKE Managing System
## Abstract
### Why?
The current Rust mangaling systems leaves out very key information, like types, etc.

## The System
for example this.
```rust
mod example {
	#[mike::export]
	pub fn demo(name: String) -> String {}
}
```
would become this.

```
MIKE (our prefix)
001 (mangling version)
001 (base type) 		 // function 
001 (type of components) // path
002 (components in path)
7 (length of component)
example (component)
4 (length of component)
demo (component)
002 (type of components) // arguments
001 (components in path)
4
name
6
String
003 (type of components) // outputs
001 (components in path)
6
String
```

*note: each component can contain multiple length-prefixed strings, this depends on the type*
