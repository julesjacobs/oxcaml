set -eu

mode="$1"
dump="$2"

if test "$mode" = absent; then
  for field in expression_type_judgments result_span scope lexical_bindings
  do
    if grep -Eq "\"$field\"[[:space:]]*:" "$dump"; then
      echo "unexpected $field in $dump"
      exit 1
    fi
  done
  exit 0
fi

compact=$(tr -d '[:space:]' < "$dump")

printf '%s' "$compact" | grep -Fq '"start":{"line":46,"column":21},"end":{"line":46,"column":26}'
printf '%s' "$compact" | grep -Fq '"type":"int","provenance":"checked"'
printf '%s' "$compact" | grep -Fq '"start":{"line":50,"column":10},"end":{"line":50,"column":11}'
printf '%s' "$compact" | grep -Fq '"start":{"line":51,"column":10},"end":{"line":51,"column":11}'
printf '%s' "$compact" | grep -Fq '"bound_identifiers":'
printf '%s' "$compact" | grep -Fq '"lexical_bindings":'

shadow_ids=$(printf '%s' "$compact" \
  | grep -o '"name":"shadow","id":"[^"]*"' \
  | sort -u \
  | wc -l)
test "$shadow_ids" -ge 2

# The guarded-case binder scope starts in the guard and reaches through the
# RHS, rather than beginning at the one-character RHS expression.
printf '%s' "$compact" | grep -Fq '"start":{"line":55,"column":34}'
printf '%s' "$compact" | grep -Fq '"end":{"line":55,"column":45}'
