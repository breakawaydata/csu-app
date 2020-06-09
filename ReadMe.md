# Breakaway Application!

## Deployment

1. Make sure all the packages from `src/.renv.lock` file are included in your local environment.
   In order to install them run `renv::restore("src/.renv.lock")`.
2. Copy `credentials` file to `deployment/`.
3. Make sure your working directory is set inside `src/` folder.
4. Run `deployment/deploy.R` file.

## Inviting new users

Log in to [Shinyapps](https://www.shinyapps.io/) dashboard.

Navigate to `Applications -> breakaway -> Users`.
Press `Invite User` and provide suitable user e-mail address.

## Development workflow

### Development user confoguration

Before you start put .Renviron file with the below content:
```
username=<your-testing-username>
```
inside `src/` folder.

### Working with dockerized workflow

Tasks are defined in `environment/tasks.py`

Run `./workflow init` to install required dependencies like pyyaml or docker-compose.
Workflow dependencies are defined in `environment/bootstrap.sh`.

Available tasks:

```
./workflow rstudio.start                 # Run full development environment
./workflow rstudio.build --tag={tag}     # Build project image based on environment/Dockerfile
./workflow rstudio.snapshot --tag={tag}  # Save project image based on the current state of a container
./workflow rstudio.down                  # Bring down the development environment
./workflow rstudio.prune                 # This will remove:
./workflow rstudio.ps                    # List running docker-compose services
./workflow rstudio.push                  # Push created images to docker registry
./workflow rstudio.r                     # Execute R shell in the rstudio container
./workflow rstudio.bash                  # Execute shell in the rstudio container
./workflow rstudio.lint                  # TODO: needs to be implemented
./workflow rstudio.styler                # TODO: needs to be implemented
./workflow rstudio.test                  # TODO: needs to be implemented
```

#### Build your development docker image

1. Development image is built using `environment/Dockerfile`. Edit it to install required system dependencies.
1. Edit `environment/workflow.yml` and choose name for your image (`image: appsilon/project` value)
1. For tagging (versioning) images we use the following convention: `MAJOR.MINOR` for example: `1.0`, `1.23`, `2.5`. Increase `MAJOR` when you do breaking changes (e.g. changing the base image). Increase `MINOR` every time you add any small change.
1. Build it: `./workflow rstudio.build --tag=1.0`

#### Install R packages

R packages are installed with **renv** which uses `src/renv.lock`.

There are 2 ways of updating your image with R packages. One is quick, using `./workflow rstudio.snapshot`, second is strict using `./workflow rstudio.build` and rebuilding the image from `environment/Dockerfile` and `src/renv.lock`.

1. Use `./workflow rstudio.start` or `./workflow rstudio.r` to start R session
1. Install your packages using `renv::install()`. Make sure you are in `/mnt/src` directory, to write to `/mnt/src/renv.lock`
1. Run `renv::snapshot()` to save changes to the `renv.lock`
1. **Quick install**: run `./workflow rstudio.snapshot --tag={yourtag}` to save the current state of your container
1. **Strict install**: run `./workflow rstudio.build --tag={yourtag}` to build image from `Dockerfile` and `renv.lock`.

#### Remember to publish your changes

1. Push your image to the registry: `./workflow rstudio.push`
1. Commit your changes in git
# Shiny pattern

Project files for Shiny app.

## Usage

Define your custom tasks in `src/tasks.py`
