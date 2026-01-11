library(ggplot2)

set.seed(123)

k <- 10
need_pos <- 2

# ---- segment intersection test (base math; no extra packages) ----
seg_intersect <- function(a, b, tol = 1e-12) {
  # a, b: named numeric vectors c(x1,y1,x2,y2)
  x1 <- a["x1"]; y1 <- a["y1"]; x2 <- a["x2"]; y2 <- a["y2"]
  x3 <- b["x1"]; y3 <- b["y1"]; x4 <- b["x2"]; y4 <- b["y2"]
  
  denom <- (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
  if (abs(denom) < tol) return(FALSE)  # parallel/collinear -> treat as non-intersecting
  
  px <- ((x1*y2 - y1*x2) * (x3 - x4) - (x1 - x2) * (x3*y4 - y3*x4)) / denom
  py <- ((x1*y2 - y1*x2) * (y3 - y4) - (y1 - y2) * (x3*y4 - y3*x4)) / denom
  
  on_seg <- function(xa, ya, xb, yb, x, y) {
    x >= min(xa, xb) - tol && x <= max(xa, xb) + tol &&
      y >= min(ya, yb) - tol && y <= max(ya, yb) + tol
  }
  
  on_seg(x1, y1, x2, y2, px, py) && on_seg(x3, y3, x4, y4, px, py)
}

# ---- build k non-intersecting segments, using distinct endpoints, with >= need_pos positive slopes ----
make_pairs <- function(df, k = 10, need_pos = 2, max_tries = 50000) {
  n <- nrow(df)
  used <- rep(FALSE, n)
  segs <- list()
  pos_count <- 0
  tries <- 0
  
  while (length(segs) < k && tries < max_tries) {
    tries <- tries + 1
    
    cand <- which(!used)
    if (length(cand) < 2) break
    idx <- sample(cand, 2)
    
    x1 <- df$price[idx[1]]; y1 <- df$sales_qty[idx[1]]
    x2 <- df$price[idx[2]]; y2 <- df$sales_qty[idx[2]]
    
    # positive slope if (y2-y1)/(x2-x1) > 0  (handle verticals safely)
    sgn <- sign((y2 - y1) * (x2 - x1))  # +1 positive, -1 negative, 0 vertical/horizontal
    
    remaining_slots <- k - length(segs)
    remaining_pos_needed <- max(0, need_pos - pos_count)
    if (remaining_slots == remaining_pos_needed && sgn != 1) next
    
    new_seg <- c(x1 = x1, y1 = y1, x2 = x2, y2 = y2)
    
    # reject if it intersects any existing segment
    ok <- TRUE
    if (length(segs) > 0) {
      for (s in segs) {
        if (seg_intersect(new_seg, s)) { ok <- FALSE; break }
      }
    }
    if (!ok) next
    
    # accept
    segs[[length(segs) + 1]] <- new_seg
    used[idx] <- TRUE
    if (sgn == 1) pos_count <- pos_count + 1
  }
  
  if (length(segs) < k) {
    warning("Could not find enough non-overlapping segments; try increasing max_tries or relaxing constraints.")
  }
  
  # convert to data frame for ggplot
  do.call(rbind, lapply(seq_along(segs), function(i) {
    s <- segs[[i]]
    data.frame(
      id = i,
      x = s["x1"], y = s["y1"],
      xend = s["x2"], yend = s["y2"],
      xm = (s["x1"] + s["x2"]) / 2,
      ym = (s["y1"] + s["y2"]) / 2
    )
  }))
}

seg_df <- make_pairs(price_demand, k = k, need_pos = need_pos)

# ---- plot ----
ggplot(price_demand, aes(price, sales_qty)) +
  geom_point(alpha = 0.5) +
  geom_segment(
    data = seg_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    linewidth = 1
  ) +
  geom_text(
    data = seg_df,
    aes(x = xm, y = ym, label = id),
    vjust = -0.6,
    size = 4
  ) +
  theme_minimal()
