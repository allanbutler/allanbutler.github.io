---
layout: default
title: Jekyll Food
---

{% for food in site.food %}

<a href="{{ food.url | prepend: site.baseurl }}">
  <h2>{{ food.title }}</h2>
</a>

<p class="post-excerpt">{{ food.description | truncate: 160 }}</p>

{% endfor %}  
