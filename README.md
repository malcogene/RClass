### How to install a package from GitHub

First, you need to install the devtools package. You can do this from CRAN. Invoke R and then type
```
install.packages("devtools")

```
Load the devtools package.
```
library(devtools)
```

```
install_github("githubID/repo")
```
the package actually sits in the pkg 'subdirectory'. To install his package with install_github(), you’d need to do:  install_github("wrengels/HWxtest", subdir="pkg")

First Header | Second Header
------------ | -------------
Content from cell 1 | Content from cell 2
Content in the first column | Content in the second column


```
# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).





