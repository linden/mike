# The MIKE Mangling System
## Abstract
### Why?
The current Rust mangling systems leaves out very key information, like types, etc.

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
_
MIKE 	(our prefix)
_
1 		(mangling version)
_
1		(symbol type)
_
2   	(components in path)
7 		(length prefix)
example (component)
4 		(length prefix)
demo 	(component)
_ 		(body)
*message packed arguments and return type*
```
