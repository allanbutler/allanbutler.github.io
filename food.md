---
layout: page
title: "A List of Food Recipes"
permalink: "/food/"
image: img/stove_shakshuka.JPG
---

{% for themes in site.themes %}

<a href="{{ food.url | prepend: site.baseurl }}">
  <h2>{{ themes.title }}</h2>
</a>

<p class="post-excerpt">{{ food.description | truncate: 160 }}</p>

{% endfor %} 

[![shakshuka]({{ site.url }}/img/shakshuka.JPG)]({{ site.url }}/img/shakshuka.JPG)
