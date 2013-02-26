from fabric.api import sudo, run, env, cd
import config

env.hosts = config.hosts
env.user = config.user
env.directory = config.directory

project_name = config.__dict__.get('project_name', 'quickdocs')


def update():
    with cd(env.directory):
        run('git pull')


def restart():
    sudo('supervisorctl restart %s' % project_name)


def setup():
    with cd(env.directory):
        run('make setup')


def bin():
    with cd(env.directory):
        run('make bin')


def deploy():
    update()
    restart()
    bin()
