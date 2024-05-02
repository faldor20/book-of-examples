# Completion

## JsonRpc

### Implementing the spec

## Lsp

### Implementing the spec
- Challenges implementing specs using json in languages not designed for it
- ?Why you should use autogeneration like ts2fs or ts2ocaml etc?
- Examples of the increasing complexity thing
  - naievly implement just a single message 
  - explain why that's bad, why you need to determine between request messages and notification messages before you do anything else. becasue otherwhise you can't get a return

### Unions
- Why We want to represent stuff as tags
#### Enum Unions
#### Request and Response decoding Unions
- Why do tags make sense for this structure? How else could it be decoded/encoded

### Options
- Why do we need a custom option type

## Automcomplete

