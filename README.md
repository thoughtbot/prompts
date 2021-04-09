# Prompts

Prompts is a small application for presenting a list of prompts from CSVs.

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

## Setup

```sh
bin/setup
```

## Running the Application

```sh
heroku local
```

## CSV format

* `Category` – Required. Comma-delimited list of categories.
* `Prompt` - Required. The prompt or question to display. Allows for one wildcard replacement, set with `Options` (`{}`)
* `Context` - Optional. Additional context to the prompt itself.
* `Options` - Optional. Comma-delimited list of options which will replace `{}` in the prompt.

### A CSV may look like this:

| Category | Prompt | Context | Options |
| -------- | ------ | ------- | ------- |
| colors, childhood | What is your favorite crayon color? | Think back to when you were 6 years old | |
| games, childhood | Did you enjoy playing {}? | | chess, checkers, poker |

## Configuration

To enable `localStorage` persistence of valid URLs, set the environment
variable `ELM_APP_ENABLE_LOCAL_STORAGE=true`.

## License

See the [LICENSE](LICENSE).
