# Creating a patch
```
# http://linux.byexamples.com/archives/163/how-to-create-patch-file-using-patch-and-diff/
# create a patch with some context
diff -cr folder1 folder2 > patch

# view the diff with color
cat patch | colordiff | less

# make sure there are no errors
patch --dry-run -p1 -i patch

# do the patch
patch -p1 -i patch
```

