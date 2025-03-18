pub fn str_matches_any(needle: &str, haystack: &[&str]) -> bool {
    for item in haystack {
        if needle == *item {
            return true;
        }
    }
    false
}