%define srcarchiveurl	http://smlnj.cs.uchicago.edu/dist/working/%{version}/
%define targetdir	/opt/%{name}

Summary:        Standard ML of New Jersey
Name:           smlnj
Version:        110.77
Release:        1
URL:            http://www.smlnj.org/
License:        http://www.smlnj.org/license.html
Group:          Development/Languages
Source:         %{srcarchiveurl}/config.tgz
Source1:        %{srcarchiveurl}/%{version}-README.html
Source2:        %{srcarchiveurl}/HISTORY
Source3:        %{srcarchiveurl}/INSTALL
Source4:        %{srcarchiveurl}/boot.x86-unix.tgz
Source5:        %{srcarchiveurl}/runtime.tgz
Source6:        %{srcarchiveurl}/MLRISC.tgz
Source7:        %{srcarchiveurl}/smlnj-lib.tgz
Source8:        %{srcarchiveurl}/ckit.tgz
Source9:        %{srcarchiveurl}/nlffi.tgz
Source10:       %{srcarchiveurl}/cml.tgz
Source11:       %{srcarchiveurl}/ml-lpt.tgz
Source12:       %{srcarchiveurl}/ml-lex.tgz
Source13:       %{srcarchiveurl}/ml-yacc.tgz
Source14:       %{srcarchiveurl}/ml-burg.tgz
Source15:       %{srcarchiveurl}/trace-debug-profile.tgz
Source16:       %{srcarchiveurl}/doc.tgz
Patch:          config-install.patch

# Only 32-bit architectures currently supported;
# see http://smlnj.org/install/ and http://www.smlnj.org/.
%ifarch x86_64
BuildRequires: glibc-devel(x86-32)
BuildRequires: libgcc(x86-32)
%else
BuildRequires: glibc-devel
BuildRequires: libgcc
%endif
BuildRequires: gcc


%description
Standard ML of New Jersey (abbreviated SML/NJ) is a compiler for the
Standard ML 97 programming language with associated libraries, tools,
and documentation.


%prep
%setup -c
%{__cp} %{SOURCE1} .
%{__cp} %{SOURCE2} .
%{__cp} %{SOURCE3} .
%{__cp} %{SOURCE4} .
%{__cp} %{SOURCE5} .
%{__cp} %{SOURCE6} .
%{__cp} %{SOURCE7} .
%{__cp} %{SOURCE8} .
%{__cp} %{SOURCE9} .
%{__cp} %{SOURCE10} .
%{__cp} %{SOURCE11} .
%{__cp} %{SOURCE12} .
%{__cp} %{SOURCE13} .
%{__cp} %{SOURCE14} .
%{__cp} %{SOURCE15} .
%{__cp} %{SOURCE16} .
# patch config/install.sh to make scripts and binaries produced
# relocatable to eventual destination %{targetdir}
%patch -p1


%build
topdir=$(pwd)
export SMLNJ_HOME="${topdir}"
export TARGETDIR="%{targetdir}"
./config/install.sh


%install
# we only need to retain bin, lib and doc
%{__rm} -rf %{buildroot}
%{__mkdir} -p %{buildroot}%{_bindir} %{buildroot}%{_mandir} %{buildroot}%{targetdir}
%{__cp} -rp bin lib %{buildroot}%{targetdir}
%{__cp} -rp doc/man/man? %{buildroot}%{_mandir}
for f in bin/*; do
    %{__ln_s} %{targetdir}/$f %{buildroot}%{_bindir}/$(basename $f)
done


%files
%defattr(-, root, root, -)
%dir %{targetdir}
%doc %{version}-README.html HISTORY INSTALL
%{targetdir}/bin
%{targetdir}/lib
%{_bindir}/*
%{_mandir}/*/*


%changelog
* Fri Nov 24 2014 Jason Gallagher <jason@slashbot.com> - 110.77-1
- Initial build
