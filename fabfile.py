from fabric.api import sudo, run, env, cd
from config import *

env.project_name = default_project_name


def update():
    with cd(env.directory):
        run('git pull')


def restart():
    sudo('supervisorctl restart %s' % env.project_name, shell=False)


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


def tail_access():
    run('tail -f /var/log/nginx/%s_access.log' % env.project_name)


def tail_error():
    run('tail -n 100 -f /var/log/apps/%s_error.log' % env.project_name)
