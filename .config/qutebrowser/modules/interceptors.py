from PyQt6.QtCore import QUrl

from qutebrowser.api import interceptor


def redirect_to_https(req: interceptor.Request):
    """
    Rewrite all HTTP requests to HTTPS, except for localhost
    """
    url = req.request_url
    # Bypass non-HTTP requests.
    if url.scheme() != "http":
        return
    # Ignore localhost.
    if url.host() in ["localhost", "127.0.0.1", "::1"]:
        return

    url.setScheme("https")

    # Try redirecting to HTTPS address.
    req.redirect(url)


def redirect_empty_search_to_base(req: interceptor.Request):
    """
    Redirect search engine URLs with empty queries to their base URL.
    """
    url = req.request_url
    url_str = url.toString(QUrl.ComponentFormattingOption.PrettyDecoded)

    # Define the search engine patterns to match with empty queries
    # Format: (url_with_empty_query, base_url)
    search_patterns = [
        ("https://music.youtube.com/search?q=", "https://music.youtube.com/"),
        ("https://www.youtube.com/results?search_query=", "https://www.youtube.com/"),
        ("https://chatgpt.com/?model=auto&q=", "https://chatgpt.com/"),
        ("https://github.com/search?q=", "https://github.com/"),
    ]  # fmt: skip

    # Check if URL exactly matches any pattern (empty query
    for pattern, base_url in search_patterns:
        if url_str == pattern:
            url.setUrl(base_url)
            req.redirect(url)
            return
