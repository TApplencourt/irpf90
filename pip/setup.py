from distutils.core import setup

setup(
  name = 'irpf90',
  packages = ['irpf90_libs'], # this must be the same as the name above
  version = '1.6.9',
  description = 'IRPF90 is a Fortran90 preprocessor written in Python for programming using the Implicit Reference to Parameters (IRP) method. It simplifies the development of large fortran codes in the field of scientific high performance computing.',
  author = 'Anthony Scemama',
  author_email = 'scemama@irsamc.ups-tlse.fr',
  url = 'http://irpf90.ups-tlse.fr', # use the URL to the github repo
  download_url = 'https://github.com/scemama/irpf90/archive/v1.6.9.tar.gz', # I'll explain this in a second
  keywords = ['programming', 'fortran', 'IRP'], # arbitrary keywords
  classifiers = [],
  scripts = ["irpf90", "irpman", "irpf90_indent"],
)

