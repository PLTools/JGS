dune exec jsons/run_json2.exe -- -n 100 -ct jsons_real/0.json only_type_queries/Serializable.json
  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../jsons_real/0.json Serializable.json -n 100
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.io.Serializable ()
    1)  Array<java.lang.Object>
    2)  java.io.Serializable
    3)  org.jooq.impl.MergeImpl$MatchedClause
    4)  org.jooq.impl.MergeImpl$NotMatchedClause
    5)  javax.swing.text.DefaultFormatter$DefaultDocumentFilter
    6)  javax.swing.text.DefaultFormatter$DefaultNavigationFilter
    7)  javax.swing.text.DefaultFormatter
    8)  javax.swing.text.DefaultFormatterFactory
    9)  net.bytebuddy.description.type.TypeDescription$ForLoadedType
   10)  javax.swing.ColorChooserDialog$Closer
   11)  javax.swing.ColorChooserDialog$DisposeOnClose
   12)  javax.swing.JMenu$MenuChangeListener
   13)  javax.swing.JMenu$WinListener
   14)  com.google.common.collect.EmptyContiguousSet$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   15)  sun.swing.table.DefaultTableCellHeaderRenderer$EmptyIcon
   16)  com.sun.java.swing.plaf.motif.MotifTreeCellRenderer$TreeLeafIcon
   17)  com.google.common.collect.RegularContiguousSet$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   18)  javax.swing.colorchooser.DefaultSwatchChooserPanel$MainSwatchListener
   19)  javax.swing.colorchooser.DefaultSwatchChooserPanel$RecentSwatchListener
   20)  javax.swing.JFormattedTextField$AbstractFormatter
   21)  javax.swing.JFormattedTextField$DocumentHandler
   22)  javax.swing.JFormattedTextField$FocusLostHandler
   23)  Array<Array<java.lang.Object>>
   24)  com.google.common.collect.ImmutableSortedMultiset$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   25)  com.google.common.collect.ImmutableAsList$SerializedForm
   26)  com.google.common.collect.ImmutableSortedSet$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   27)  Array<Intersect [java.lang.Object | _.3530]>
   28)  com.google.common.collect.ImmutableMultiset$SerializedForm
   29)  com.google.common.collect.ImmutableMapKeySet$KeySetSerializedForm<[Not implemented JGS_Helpers.ml 74]>
   30)  Array<Not implemented JGS_Helpers.ml 74>
   31)  com.google.common.collect.RegularImmutableMap$KeySet$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   32)  sun.security.pkcs11.SunPKCS11$SunPKCS11Rep
   33)  Array<null>
   34)  com.sun.java.swing.plaf.motif.MotifRadioButtonMenuItemUI$ChangeHandler
   35)  com.sun.org.apache.xerces.internal.dom.DocumentImpl$EnclosingAttr
   36)  com.sun.org.apache.xerces.internal.dom.DocumentImpl$LEntry
   37)  com.sun.rowset.FilteredRowSetImpl
   38)  javax.swing.text.html.parser.ParserDelegator
   39)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; java.lang.Object | _.8268]>
   40)  javax.swing.JMenuItem$MenuItemFocusListener
   41)  javax.swing.plaf.basic.BasicComboPopup$EmptyListModelClass
   42)  javax.swing.plaf.basic.BasicComboPopup$Handler
   43)  Array<org.jooq.impl.SelectImpl$1>
   44)  javax.swing.JTextField$ScrollRepainter
   45)  java.beans.beancontext.BeanContextServicesSupport$BCSSServiceProvider
   46)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; java.lang.Object | _.9761]>
   47)  com.google.common.collect.ImmutableList$SerializedForm
   48)  com.google.common.collect.ImmutableSet$SerializedForm
  ERROR: free variables inside class id
   49)  com.google.common.collect.EmptyContiguousSet$SerializedForm<[_.??]>
   50)  java.util.concurrent.SynchronousQueue$WaitQueue
   51)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; java.lang.Object | _.11463]>
   52)  Array<org.jooq.impl.CreateTableImpl$1>
  ERROR: free variables inside class id
   53)  com.google.common.collect.RegularContiguousSet$SerializedForm<[_.??]>
   54)  Array<java.lang.Object>
   55)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; java.lang.Object | _.13145]>
   56)  com.google.common.collect.TreeMultiset<[Not implemented JGS_Helpers.ml 74]>
   57)  Array<kotlin.text.StringsKt___StringsKt$asIterable$$inlined$Iterable$1>
  ERROR: free variables inside class id
   58)  com.google.common.collect.ImmutableSortedMultiset$SerializedForm<[_.??]>
   59)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; _.13147 =/= [ java.lang.Object ]; java.lang.Object | _.14782]>
   60)  com.sun.org.apache.xpath.internal.axes.UnionPathIterator
   61)  com.google.common.collect.RegularImmutableMap$Values$SerializedForm<[Not implemented JGS_Helpers.ml 74]>
   62)  javax.swing.text.JTextComponent$ComposedTextCaret
   63)  javax.swing.text.html.HTMLEditorKit$LinkController
   64)  com.google.common.collect.ImmutableMultiset$EntrySetSerializedForm<[Not implemented JGS_Helpers.ml 74]>
  ERROR: free variables inside class id
   65)  com.google.common.collect.ImmutableSortedSet$SerializedForm<[_.??]>
   66)  javax.swing.text.html.HiddenTagView$EndTagBorder
   67)  javax.swing.text.html.HiddenTagView$StartTagBorder
   68)  com.google.common.collect.ImmutableMultimap$KeysSerializedForm
   69)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; _.13147 =/= [ java.lang.Object ]; _.14784 =/= [ java.lang.Object ]; java.lang.Object | _.16488]>
   70)  com.google.common.collect.ImmutableEnumSet$EnumSerializedForm<[Not implemented JGS_Helpers.ml 74]>
  ERROR: free variables inside class id
   71)  com.google.common.collect.ImmutableMapKeySet$KeySetSerializedForm<[_.??]>
   72)  javax.swing.AbstractButton$ButtonChangeListener
   73)  Array<kotlin.text.StringsKt___StringsKt$asSequence$$inlined$Sequence$1>
   74)  javax.swing.AbstractButton$Handler
   75)  java.util.Collections$UnmodifiableNavigableSet$EmptyNavigableSet<[Not implemented JGS_Helpers.ml 74]>
   76)  javax.swing.JComboBox$DefaultKeySelectionManager
   77)  javax.swing.JFileChooser$FCHierarchyListener
  ERROR: free variables inside class id
   78)  com.google.common.collect.RegularImmutableMap$KeySet$SerializedForm<[_.??]>
   79)  javax.swing.DefaultListCellRenderer
   80)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; _.13147 =/= [ java.lang.Object ]; _.14784 =/= [ java.lang.Object ]; _.16490 =/= [ java.lang.Object ]; java.lang.Object | _.18205]>
   81)  javax.swing.plaf.basic.BasicComboBoxRenderer
   82)  javax.swing.table.DefaultTableCellRenderer
   83)  javax.swing.JList$ListSelectionHandler
   84)  javax.swing.JProgressBar$ModelListener
   85)  javax.swing.JRootPane$RootLayout
   86)  javax.swing.JScrollBar$ModelListener
   87)  Array<kotlin.text.StringsKt___StringsKt$groupingBy$1>
   88)  javax.swing.JSlider$ModelListener
   89)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; _.13147 =/= [ java.lang.Object ]; _.14784 =/= [ java.lang.Object ]; _.16490 =/= [ java.lang.Object ]; _.18207 =/= [ java.lang.Object ]; java.lang.Object | _.19785]>
   90)  javax.swing.JSpinner$ModelListener
   91)  javax.swing.JTabbedPane$ModelListener
   92)  javax.swing.JTabbedPane$Page
   93)  javax.swing.JToolBar$DefaultToolBarLayout
   94)  Array<org.jooq.impl.SelectImpl$1>
   95)  javax.swing.JTree$TreeSelectionRedirector
   96)  javax.swing.JViewport$ViewListener
   97)  Array<Intersect [_.3532 =/= [ java.lang.Object ]; _.8270 =/= [ java.lang.Object ]; _.9763 =/= [ java.lang.Object ]; _.11465 =/= [ java.lang.Object ]; _.13147 =/= [ java.lang.Object ]; _.14784 =/= [ java.lang.Object ]; _.16490 =/= [ java.lang.Object ]; _.18207 =/= [ java.lang.Object ]; _.19787 =/= [ java.lang.Object ]; java.lang.Object | _.21600]>
   98)  Array<org.jooq.impl.MergeImpl$1>
   99)  com.google.common.reflect.TypeToken$TypeSet
   100)  java.beans.beancontext.BeanContextSupport$BCSChild
