"""

Input Tree
       A
      / \
     B   C
    / \   \
   D   E   F


Output Tree
       A--->NULL
      / \
     B-->C-->NULL
    / \   \
   D-->E-->F-->NULL

[    [ A ],
    [ B, C ],
  [D, E, _, F],
]

Rotate Tree

       D
      / \
     E   B
    /   / \
   F   C   A


Inorder traversal of input:  [D B E A C F]
Preorder traversal of input: [A B D E C F]  -> B A F C E D
Postordr traversal of input: [D E B C F A]

Inorder traversal of rotate: [F E D C B A]  -> B A F E D C

"""
