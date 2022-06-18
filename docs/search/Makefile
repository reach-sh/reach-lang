IMAGE=docs-search

.PHONY: run
run:
	docker build -f Dockerfile --tag=$(IMAGE) .
	docker run --rm -e ALGOLIA_KEY -e GITHUB_GRAPHQL_KEY $(IMAGE)
