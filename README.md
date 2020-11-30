<pre>
$ mkdir -p a/a0/a00 a/a0/a01
$ mkdir -p b/b0 b/b1/b10
$ touch a/file a/a0/file a/a0/a00/file
$ touch b/file b/b0/file b/b1/b10/file
$ tree a b
a
├── a0
│   ├── a00
│   │   └── file
│   ├── a01
│   └── file
└── file
b
├── b0
│   └── file
├── b1
│   └── b10
│       └── file
└── file

6 directories, 6 files
<b>$ skelet a b/ c</b>
$ tree c
c
├── a
│   └── a0
│       ├── a00
│       └── a01
├── b0
└── b1
    └── b10

7 directories, 0 files
</pre>
