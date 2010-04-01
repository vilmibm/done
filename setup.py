from setuptools import setup

from done import get_version

setup(
    name='done',
    version=get_version(),
    url='http://github.com/nathanielksmith/done',
    description='simple, elegant command line todo list tool',
    author='Nathaniel K Smith',
    author_email='nathanielksmith@gmail.com',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License (GPL)',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Utilities',
    ],
    packages=['done', 'sql_interp', 'parsedatetime'],
    entry_points={'console_scripts': [
        'd = done.done:main']},
)
