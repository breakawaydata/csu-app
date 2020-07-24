from invoke import task, Collection

ns = Collection('unit-tests')

@task
def run(c):
    """Run unit tests"""
    c.run("docker-compose exec -T rstudio bash -c 'cd /mnt/src; Rscript --no-init-file tests/testthat.R'", echo=True)
ns.add_task(run)
