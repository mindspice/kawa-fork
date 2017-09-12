## Process this with automake to create Makefile.in

java_sources = \
Access.java \
AnnotationEntry.java \
ArrayClassLoader.java \
ArrayType.java \
AttrContainer.java \
Attribute.java \
ClassType.java \
CodeAttr.java \
ExceptionsAttr.java \
ExitableBlock.java \
Field.java \
Filter.java \
IfState.java \
Label.java \
ListCodeSize.java \
LocalVarsAttr.java \
Location.java \
Member.java \
Method.java \
ObjectType.java \
ParameterizedType.java \
PrimType.java \
RuntimeAnnotationsAttr.java \
Scope.java \
SourceFileAttr.java \
SpecialObjectType.java \
SwitchState.java \
TryState.java \
Type.java \
TypeVariable.java \
UninitializedType.java \
VarEnumerator.java \
Variable.java \
ZipArchive.java \
ZipLoader.java

PACKAGE_FNAME = gnu-bytecode
GCJ_INPUTS =  $(java_Java)

EXTRA_DIST = package.html

include $(top_srcdir)/Make-rules

all: Makefile java-classes.stamp $(@GCJ_COMPILED_SELECTED@)
