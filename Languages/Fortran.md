---
title: Fortran
aliases:
  - FORTRAN
  - Formula Translation
tags:
  - language
  - legacy
  - scientific
  - numerical
  - hpc
type: reference
status: complete
created: "2025-12-16"
---

# Fortran

The oldest high-level programming language, still dominant in scientific computing and high-performance numerical simulation.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Procedural, array-oriented, OOP (2003+) |
| **Typing** | Static, strong |
| **Memory Model** | Automatic arrays, allocatable, pointers |
| **First Appeared** | 1957 (John Backus, IBM) |
| **Current Standard** | Fortran 2023 |
| **Primary Use Cases** | Scientific simulation, HPC, numerical computing |
| **Compilers** | gfortran, Intel Fortran, NVIDIA HPC, NAG |
| **Key Domains** | Physics, chemistry, weather, climate, CFD |

## Core Concepts

### Array-First Design

**Native Multidimensional Arrays** — First-class support for numerical computing.

```fortran
real :: matrix(100, 100)
real :: vector(1000)

! Whole-array operations
matrix = 0.0
vector = vector * 2.0

! Array slicing
matrix(1:10, :) = 1.0
```

**Column-Major Order** — Arrays stored column-first (opposite of C).

### Array Operations

**Intrinsic Functions** — Operate on entire arrays:

```fortran
sum(array)           ! Sum all elements
maxval(array)        ! Maximum value
matmul(A, B)         ! Matrix multiplication
transpose(matrix)    ! Matrix transpose
dot_product(a, b)    ! Vector dot product
```

**WHERE Statement** — Conditional array operations:

```fortran
where (temperature > 100.0)
    state = "gas"
elsewhere
    state = "liquid"
end where
```

### Program Structure

```fortran
program heat_simulation
    implicit none
    real :: temperature(100, 100)
    integer :: i, j

    call initialize(temperature)
    call simulate(temperature, 1000)
    call output_results(temperature)

end program heat_simulation

subroutine initialize(grid)
    real, intent(out) :: grid(:,:)
    grid = 20.0  ! Room temperature
end subroutine initialize
```

### Modules

**Encapsulation and Namespacing** — Modern code organization.

```fortran
module physics_constants
    implicit none
    real, parameter :: pi = 3.14159265359
    real, parameter :: c = 299792458.0      ! Speed of light
    real, parameter :: G = 6.67430e-11      ! Gravitational constant
end module physics_constants

program simulation
    use physics_constants
    ! pi, c, G now available
end program
```

### Derived Types

**User-Defined Types** — Struct-like data structures.

```fortran
type :: particle
    real :: position(3)
    real :: velocity(3)
    real :: mass
end type particle

type(particle) :: p
p%mass = 1.0
p%position = [0.0, 0.0, 0.0]
```

## Fortran Evolution

| Version | Year | Key Features |
|---------|------|--------------|
| **FORTRAN** | 1957 | First high-level language, arithmetic expressions |
| **FORTRAN IV** | 1962 | Logical IF, subprograms |
| **FORTRAN 77** | 1977 | Structured IF-THEN-ELSE, CHARACTER type |
| **Fortran 90** | 1990 | Free-form, modules, array syntax, allocatable |
| **Fortran 95** | 1995 | FORALL, pure/elemental procedures |
| **Fortran 2003** | 2003 | OOP, C interoperability, IEEE floating-point |
| **Fortran 2008** | 2008 | Coarrays, submodules, DO CONCURRENT |
| **Fortran 2018** | 2018 | Enhanced coarrays, C interop improvements |
| **Fortran 2023** | 2023 | Generics (templates), enumeration types |

**Naming Convention:**

- FORTRAN (all caps) — Versions before 90
- Fortran (capitalized) — Modern versions (90+)

## Parallel Computing

### Coarrays

**Built-In Parallelism** — Native distributed memory model.

```fortran
real :: local_sum[*]  ! Coarray - one per image
real :: global_sum

local_sum = sum(my_data)
sync all
if (this_image() == 1) then
    global_sum = sum(local_sum[1:num_images()])
end if
```

### DO CONCURRENT

**Parallel Loops** — Compiler-optimized parallel iteration.

```fortran
do concurrent (i = 1:n, j = 1:m)
    result(i,j) = compute(data(i,j))
end do
```

### OpenMP Integration

**Directive-Based Parallelism:**

```fortran
!$omp parallel do
do i = 1, n
    a(i) = b(i) + c(i)
end do
!$omp end parallel do
```

### MPI

**Message Passing** — Industry standard for distributed computing.

```fortran
use mpi
call MPI_Init(ierr)
call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
call MPI_Send(data, count, MPI_REAL, dest, tag, MPI_COMM_WORLD, ierr)
call MPI_Finalize(ierr)
```

## Scientific Libraries

| Library | Domain |
|---------|--------|
| **BLAS** | Basic Linear Algebra Subprograms |
| **LAPACK** | Linear algebra (eigenvalues, SVD, systems) |
| **ScaLAPACK** | Distributed LAPACK |
| **FFTW** | Fast Fourier transforms |
| **PETSc** | Parallel solvers |
| **NetCDF** | Scientific data format I/O |
| **HDF5** | Hierarchical data format |

**Performance Note:** Fortran's array model allows compilers to generate highly optimized SIMD code.

## Compilers

| Compiler | Vendor | Notes |
|----------|--------|-------|
| **gfortran** | GNU | Free, widely available |
| **ifort/ifx** | Intel | Highly optimized for Intel CPUs |
| **nvfortran** | NVIDIA | GPU offloading support |
| **NAG Fortran** | NAG | Excellent standards compliance |
| **Flang** | LLVM | Modern LLVM-based compiler |

**Recommended Flags (gfortran):**

- `-O2` / `-O3` — Optimization
- `-Wall -Wextra` — Warnings
- `-fcheck=all` — Runtime checks (debug)
- `-fopenmp` — OpenMP support
- `-std=f2018` — Standard compliance

## Comparison with Alternatives

| Aspect | Fortran | C/C++ | Python (NumPy) | Julia |
|--------|---------|-------|----------------|-------|
| **Array Syntax** | ✅ Native, elegant | ❌ Manual loops | ✅ NumPy arrays | ✅ Native arrays |
| **Performance** | ✅ Excellent | ✅ Excellent | ⚠️ C extensions | ✅ JIT compiled |
| **Parallel** | ✅ Coarrays, OpenMP | ⚠️ Libraries | ⚠️ Limited | ✅ Native tasks |
| **Legacy Code** | ✅ Decades of libraries | ⚠️ Some | ❌ None | ❌ None |
| **Learning Curve** | ⚠️ Unusual syntax | ⚠️ Complex | ✅ Easy | ✅ Easy |
| **Modern Features** | ⚠️ Catching up | ✅ Rapid evolution | ✅ Extensive | ✅ Modern design |

### Key Distinctions

**Fortran vs C/C++:**

- Fortran arrays are bounds-checked (optionally), C is not
- Fortran uses column-major, C uses row-major
- Fortran has no pointers in the C sense (safer)
- Fortran's complex number type is native

**Fortran vs Python/NumPy:**

- Fortran compiles to native code (much faster)
- NumPy backends often call Fortran (BLAS/LAPACK)
- Python better for prototyping, Fortran for production HPC

**Fortran vs Julia:**

- Julia is newer with modern syntax
- Fortran has 60+ years of optimized libraries
- Julia's JIT adds startup overhead

## When to Use Fortran

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Numerical Performance** | Compilers generate highly optimized numerical code |
| **Array Operations** | First-class multidimensional arrays |
| **Scientific Libraries** | BLAS, LAPACK, decades of validated code |
| **HPC Ecosystem** | Native parallelism, MPI/OpenMP integration |
| **Backwards Compatibility** | Old code still compiles and runs |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Syntax** | Different from mainstream languages |
| **String Handling** | Historically awkward |
| **General Programming** | Not suited for web, apps, GUIs |
| **Declining Popularity** | Smaller community than Python/C++ |

### Best For

- **Climate Modeling** — Weather prediction, earth sciences
- **Computational Physics** — Particle simulations, quantum mechanics
- **Computational Chemistry** — Molecular dynamics, DFT
- **Aerospace/CFD** — Fluid dynamics, structural analysis
- **Financial Modeling** — Risk analysis, derivatives pricing
- **HPC Centers** — Supercomputer workloads

**Avoid For:**

- Web development
- Mobile applications
- System programming
- General application development

## Industry Usage

| Domain | Examples |
|--------|----------|
| **Weather** | ECMWF, NOAA models |
| **Physics** | CERN simulations |
| **Aerospace** | NASA, Boeing CFD |
| **Energy** | Nuclear reactor simulations |
| **Finance** | Quantitative modeling |

## Related

- [[COBOL]] — Contemporary business-oriented language
- [[C]] — Systems programming alternative
- [[Python]] — Modern scientific computing (with NumPy)
- [[Languages MOC]] — Overview of all language references
