### Setup

Create `.env` file with

```
export AOC_SESSION=abcdef
```

as instructed in `advent-of-code-data` [readme](https://pypi.org/project/advent-of-code-data/). Session value can be found in `session` cookie on [adventofcode.com](https://adventofcode.com).

Then do the following to set up Python 3.8 and install dependencies.

```sh
pipenv install
pipenv shell
pip install -r requirements.txt
source .env
```

```
sudo apt install entr
```

### Running

To run a given day's script with a watcher: `./run-day 8`

To run the tests: `pytest` or with a watcher `ptw`
