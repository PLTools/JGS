dune exec jsons/run_json2.exe -- -n 100 -ct jsons_real/0.json only_type_queries/Cloneable.json
  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../jsons_real/0.json Cloneable.json -n 100
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.lang.Cloneable ()
    1)  Array<java.lang.Object>
    2)  java.lang.Cloneable
    3)  javax.swing.text.DefaultFormatter
    4)  com.sun.rowset.FilteredRowSetImpl
    5)  com.sun.org.apache.xpath.internal.axes.UnionPathIterator
    6)  com.sun.org.apache.xpath.internal.objects.XRTreeFragSelectWrapper
    7)  com.sun.org.apache.xpath.internal.axes.AxesWalker
    8)  com.sun.org.apache.xpath.internal.axes.LocPathIterator
    9)  sun.security.ssl.DTLSInputRecord$HandshakeFlight
   10)  com.sun.rowset.CachedRowSetImpl
   11)  java.util.LinkedList<[Not implemented JGS_Helpers.ml 74]>
   12)  com.sun.org.apache.xalan.internal.xsltc.dom.MultiValuedNodeHeapIterator$HeapNode
   13)  java.util.LinkedHashSet<[Not implemented JGS_Helpers.ml 74]>
   14)  java.security.MessageDigest$Delegate$CloneableDelegate
   15)  java.security.Signature$Delegate$CloneableDelegate
   16)  java.util.TreeSet<[Not implemented JGS_Helpers.ml 74]>
   17)  com.sun.org.apache.xpath.internal.axes.NodeSequence
   18)  java.util.concurrent.ConcurrentSkipListSet<[Not implemented JGS_Helpers.ml 74]>
   19)  com.sun.org.apache.xpath.internal.objects.XRTreeFrag
   20)  com.sun.imageio.plugins.jpeg.DHTMarkerSegment$Htable
   21)  Array<Array<java.lang.Object>>
   22)  com.sun.imageio.plugins.jpeg.DQTMarkerSegment$Qtable
   23)  javax.swing.text.InternationalFormatter
   24)  com.sun.imageio.plugins.jpeg.JFIFMarkerSegment$JFIFThumb
   25)  java.util.ArrayList<[Not implemented JGS_Helpers.ml 74]>
   26)  Array<Intersect [java.lang.Object | _.3471]>
   27)  com.sun.imageio.plugins.jpeg.SOFMarkerSegment$ComponentSpec
   28)  com.sun.imageio.plugins.jpeg.SOSMarkerSegment$ScanComponentSpec
   29)  java.util.Vector<[Not implemented JGS_Helpers.ml 74]>
   30)  javax.swing.text.MaskFormatter
   31)  sun.text.RuleBasedBreakIterator$SafeCharIterator
   32)  Array<Not implemented JGS_Helpers.ml 74>
   33)  sun.java2d.SunGraphics2D
   34)  java.util.EnumSet<[Not implemented JGS_Helpers.ml 74]>
   35)  sun.print.PeekGraphics
   36)  Array<null>
   37)  java.util.HashSet<[Not implemented JGS_Helpers.ml 74]>
   38)  org.junit.jupiter.params.shadow.com.univocity.parsers.common.fields.ExcludeFieldNameSelector
   39)  org.junit.jupiter.params.shadow.com.univocity.parsers.common.fields.FieldNameSelector
   40)  java.util.ArrayDeque<[Not implemented JGS_Helpers.ml 74]>
   41)  sun.security.x509.NameConstraintsExtension
   42)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; java.lang.Object | _.8302]>
   43)  javax.swing.plaf.nimbus.NimbusStyle$RuntimeState
   44)  com.sun.org.apache.xpath.internal.objects.XObject
   45)  javax.swing.AbstractAction
   46)  Array<org.jooq.impl.SelectImpl$1>
   47)  javax.management.MBeanAttributeInfo
   48)  javax.management.MBeanConstructorInfo
   49)  javax.management.MBeanNotificationInfo
   50)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; java.lang.Object | _.9762]>
   51)  javax.management.MBeanOperationInfo
   52)  javax.management.MBeanParameterInfo
   53)  com.sun.org.apache.xpath.internal.NodeSetDTM
   54)  com.sun.rowset.internal.InsertRow
   55)  com.sun.rowset.internal.Row
  ERROR: free variables inside class id
   56)  java.util.LinkedList<[_.??]>
   57)  java.awt.image.BufferedImageFilter
   58)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; _.9764 =/= [ java.lang.Object ]; java.lang.Object | _.11474]>
   59)  javax.swing.text.Segment
   60)  java.util.concurrent.CopyOnWriteArrayList<[Not implemented JGS_Helpers.ml 74]>
   61)  Array<org.jooq.impl.CreateTableImpl$1>
   62)  javax.swing.text.ElementIterator$StackItem
   63)  com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl
  ERROR: free variables inside class id
   64)  java.util.LinkedHashSet<[_.??]>
   65)  Array<java.lang.Object>
   66)  org.jooq.conf.InterpreterSearchSchema
   67)  org.jooq.conf.MappedCatalog
   68)  org.jooq.conf.MappedSchema
   69)  org.jooq.conf.MappedTable
   70)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; _.9764 =/= [ java.lang.Object ]; _.11476 =/= [ java.lang.Object ]; java.lang.Object | _.13253]>
  ERROR: free variables inside class id
   71)  java.util.TreeSet<[_.??]>
   72)  org.jooq.conf.ParseSearchSchema
   73)  org.jooq.conf.RenderFormatting
   74)  org.jooq.conf.RenderMapping
   75)  org.jooq.conf.Settings
  ERROR: free variables inside class id
   76)  java.util.concurrent.ConcurrentSkipListSet<[_.??]>
   77)  Array<kotlin.text.StringsKt___StringsKt$asIterable$$inlined$Iterable$1>
   78)  org.postgresql.geometric.PGbox
   79)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; _.9764 =/= [ java.lang.Object ]; _.11476 =/= [ java.lang.Object ]; _.13255 =/= [ java.lang.Object ]; java.lang.Object | _.14917]>
   80)  org.postgresql.geometric.PGcircle
   81)  org.postgresql.geometric.PGline
   82)  org.postgresql.geometric.PGlseg
   83)  org.postgresql.geometric.PGpath
   84)  org.postgresql.geometric.PGpoint
   85)  org.postgresql.geometric.PGpolygon
  ERROR: free variables inside class id
   86)  java.util.ArrayList<[_.??]>
   87)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; _.9764 =/= [ java.lang.Object ]; _.11476 =/= [ java.lang.Object ]; _.13255 =/= [ java.lang.Object ]; _.14919 =/= [ java.lang.Object ]; java.lang.Object | _.16618]>
   88)  org.postgresql.util.PGInterval
   89)  org.postgresql.util.PGmoney
   90)  javax.swing.text.GlyphView
   91)  Array<kotlin.text.StringsKt___StringsKt$asSequence$$inlined$Sequence$1>
   92)  com.sun.org.apache.bcel.internal.classfile.FieldOrMethod
  ERROR: free variables inside class id
   93)  java.util.Vector<[_.??]>
   94)  com.sun.org.apache.bcel.internal.classfile.JavaClass
   95)  com.sun.org.apache.bcel.internal.generic.ClassGen
   96)  com.sun.org.apache.bcel.internal.generic.FieldGenOrMethodGen
   97)  Array<Intersect [_.3473 =/= [ java.lang.Object ]; _.8304 =/= [ java.lang.Object ]; _.9764 =/= [ java.lang.Object ]; _.11476 =/= [ java.lang.Object ]; _.13255 =/= [ java.lang.Object ]; _.14919 =/= [ java.lang.Object ]; _.16620 =/= [ java.lang.Object ]; java.lang.Object | _.18309]>
   98)  com.sun.org.apache.xerces.internal.impl.xpath.XPath$Axis
  ERROR: free variables inside class id
   99)  java.util.EnumSet<[_.??]>
   100)  com.sun.org.apache.xerces.internal.impl.xpath.XPath$LocationPath
