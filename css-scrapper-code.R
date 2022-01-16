#Using a CSS selector to extract data from a website (https://selectorgadget.com/) - install this extension

g <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- g %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- g %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- g %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

#You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
guacamole <- list(recipe, prep_time, ingredients)
guacamole

#Recipe pages from this website follow this general layout, create a function that extracts this information
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

#and then use it on any of their webpages:
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")