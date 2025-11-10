def get_game [input: string] {
    # Splitting the string into two parts
    let parts = ($input | split row ":")

    # Getting the first part ("Game 1")
    let part1 = ($parts.0 | str trim)

    # Getting the second part and trimming it
    let part2 = ($parts.1 | str trim)

    return [$part1 $part2]
}


def split_rounds [input: string] {
    let parts = ($input | split row ";")
    ($parts | each { |it| (str trim $it) | table })
}