# health-search

This is the project for INB344's main project. The project is to develop a search engine which will retrieve health-related documents based on queries. It is implemented in clojure using the `elastisch` library, which means the search engine backend is elastic search.

Configuration of the project is dine through the `config.edn` file.

## Installation

Once elasticsearch is installed, edit `elasticserach.yml` and configure `http.max_initial_line_length` to be something higher than 4kb.

## Usage

    $ java -jar health-search-1.0.jar [action] [options]

## Options

```
index        [path to document collection]
query        [search term]
bulk-query   [input queries] [output file]
```

## License

Copyright © 2015 Harry Scells

Distributed under the MIT License.
